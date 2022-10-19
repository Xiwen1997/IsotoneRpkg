#include <Rcpp.h>
# include <iostream>
# include <cmath>
# include <set>
# include <algorithm>
# include <iterator>
using namespace Rcpp;
using namespace std;

class node {
public:
  node(int i) {
    index = i;
  }
  int index = 0;
  node* next = NULL;
};

class Adjacency_list {
public:
  Adjacency_list(int size) {
    n = size;
    node_ad_array = new node * [size];
    for (int i = 0;i < size;i++) {
      node_ad_array[i] = new node(i);
    }
  }
  ~Adjacency_list() {
    for (int i = 0; i < n;i++) {
      node* cur_node = node_ad_array[i];
      node* temp_node = cur_node;
      while (cur_node->next != NULL) {
        temp_node = cur_node->next;
        delete cur_node;
        cur_node = temp_node;
      }
      delete cur_node;
    }
    delete[]node_ad_array;
  }

  Adjacency_list(const Adjacency_list& src_obj) {
    n = src_obj.n;
    node_ad_array = new node * [n];
    for (int i = 0;i < n;i++) {
      node_ad_array[i] = new node(i);
    }
    int ii = 0;
    for (int ii = 0;ii < n;ii++) {
      node* cur_node_obj = src_obj.node_ad_array[ii];
      node* cur_node = node_ad_array[ii];

      cur_node_obj = cur_node_obj->next;
      while (cur_node_obj != NULL) {
        cur_node->next = new node(cur_node_obj->index);
        cur_node_obj = cur_node_obj->next;
        cur_node = cur_node->next;
      }
    }

  }

  bool isEmpty(int i) {
    // check whether node i is connected with any other node
    return (node_ad_array[i]->next == NULL) ? true : false;
  }

  void connect(int i, int j) {
    // Connecting node i and node j
    if (node_ad_array[i]->next == NULL) {
      node_ad_array[i]->next = new node(j);
    }
    else {
      node* cur_node = node_ad_array[i]->next;
      while (cur_node->next != NULL && cur_node->index < j) {
        cur_node = cur_node->next;
      }
      node* temp_node = cur_node->next;
      cur_node->next = new node(j);
      cur_node->next->next = temp_node;
    }

    if (node_ad_array[j]->next == NULL) {
      node_ad_array[j]->next = new node(i);
    }
    else {
      node* cur_node = node_ad_array[j]->next;
      while (cur_node->next != NULL && cur_node->index < i) {
        cur_node = cur_node->next;
      }
      node* temp_node = cur_node->next;
      cur_node->next = new node(i);
      cur_node->next->next = temp_node;
    }
  }
  void disconnect(int i, int j) {
    // Disconnecting node i and node j
    bool not_connected = true;
    node* cur_node = node_ad_array[i]->next;
    node* pre_node = node_ad_array[i];
    while (cur_node->index != j) {
      pre_node = cur_node;
      cur_node = cur_node->next;
      if (cur_node == NULL) {
        not_connected = false;
        break;
      }
    }
    if (not_connected) {
      pre_node->next = cur_node->next;
      delete cur_node;
      cur_node = node_ad_array[j]->next;
      pre_node = node_ad_array[j];
      while (cur_node->index != i) {
        pre_node = cur_node;
        cur_node = cur_node->next;
      }
      pre_node->next = cur_node->next;
      delete cur_node;
    }

  }
  set<int> compute_subgraph(int i) {
    // Find all the subgraph where the node i is in.
    set<int> searched_set;
    set<int> subgraph_set;
    // initialize searched set and subgraph set
    searched_set.insert(i);
    subgraph_set.insert(i);
    node* cur_node = node_ad_array[i]->next;
    while (cur_node != NULL) {
      subgraph_set.insert(cur_node->index);
      cur_node = cur_node->next;
    }

    while (searched_set.size() != subgraph_set.size()) {
      set<int> to_search_set(subgraph_set);
      set_difference(subgraph_set.begin(), subgraph_set.end(), searched_set.begin(), searched_set.end(), inserter(to_search_set, to_search_set.begin()));
      searched_set = subgraph_set;
      for (set<int>::iterator it = to_search_set.begin();it != to_search_set.end();it++) {
        cur_node = node_ad_array[*it]->next;
        while (cur_node != NULL) {
          subgraph_set.insert(cur_node->index);
          cur_node = cur_node->next;
        }
      }
    }
    return subgraph_set;
  }
  void print() {
    for (int i = 0;i < n;i++) {
      cout << i << " -> ";
      node* cur_node = node_ad_array[i]->next;
      if (cur_node == NULL) {
        cout << "()" << endl;
      }
      else {
        cout << "( ";
        while (cur_node != NULL) {
          cout << cur_node->index << " ";
          cur_node = cur_node->next;
        }
        cout << ")" << endl;
      }
    }
  }
  bool is_directly_connected(int i, int j) {
    bool connected = false;
    node* cur_node = node_ad_array[i]->next;
    while (cur_node != NULL) {
      if (cur_node->index == j) {
        connected = true;
        break;
      }
      cur_node = cur_node->next;
    }
    return connected;
  }
  bool is_connected(int i, int j) {
    set<int> searched_set;
    set<int> subgraph_set;
    // initialize searched set and subgraph set
    searched_set.insert(i);
    subgraph_set.insert(i);
    node* cur_node = node_ad_array[i]->next;
    while (cur_node != NULL) {
      subgraph_set.insert(cur_node->index);
      if (j == cur_node->index) {
        return true;
      }
      cur_node = cur_node->next;
    }

    while (searched_set.size() != subgraph_set.size()) {
      set<int> to_search_set(subgraph_set);
      set_difference(subgraph_set.begin(), subgraph_set.end(), searched_set.begin(), searched_set.end(), inserter(to_search_set, to_search_set.begin()));
      searched_set = subgraph_set;
      for (set<int>::iterator it = to_search_set.begin();it != to_search_set.end();it++) {
        cur_node = node_ad_array[*it]->next;
        while (cur_node != NULL) {
          subgraph_set.insert(cur_node->index);
          if (j == cur_node->index) {
            return true;
          }
          cur_node = cur_node->next;
        }
      }
    }
    return false;
  }

  int n; // number of nodes
  node** node_ad_array; // the address of each node
};

class DAG_node {
public:
  DAG_node(int i) {
    index = i;
  }
  int index = 0;
  DAG_node* next = NULL;
  DAG_node* prev = NULL;
};

class DAG {
public:
  ~DAG() {
    for (int i = 0; i < n;i++) {
      DAG_node* cur_node = node_array[i];
      DAG_node* temp_node = cur_node;
      while (cur_node->next != NULL) {
        temp_node = cur_node->next;
        delete cur_node;
        cur_node = temp_node;
      }
      temp_node = cur_node;
      while (cur_node->prev != NULL) {
        temp_node = cur_node->prev;
        delete cur_node;
        cur_node = temp_node;
      }
      delete cur_node;
    }
    delete[]node_array;
  }

  void initialize(int size) {
    n = size;
    node_array = new DAG_node * [size];
    for (int i = 0;i < size;i++) {
      node_array[i] = new DAG_node(i);
    }
  }

  void connect(int i, int j) {
    if (node_array[i]->next == NULL) {
      node_array[i]->next = new DAG_node(j);
    }
    else {
      DAG_node* cur_node = node_array[i]->next;
      while (cur_node->next != NULL && cur_node->index < j) {
        cur_node = cur_node->next;
      }
      DAG_node* temp_node = cur_node->next;
      cur_node->next = new DAG_node(j);
      cur_node->next->next = temp_node;
    }

    if (node_array[j]->prev == NULL) {
      node_array[j]->prev = new DAG_node(i);
    }
    else {
      DAG_node* cur_node = node_array[j]->prev;
      while (cur_node->prev != NULL && cur_node->index < i) {
        cur_node = cur_node->prev;
      }
      DAG_node* temp_node = cur_node->prev;
      cur_node->prev = new DAG_node(i);
      cur_node->prev->prev = temp_node;
    }
  }

  bool is_connected(int i, int j) {
    bool connected = false;
    DAG_node* cur_node = node_array[i]->next;
    while (cur_node != NULL) {
      if (cur_node->index == j) {
        connected = true;
        break;
      }
      cur_node = cur_node->next;
    }
    return connected;
  }

  set<int> get_children_set(int i) {
    set<int> children_set;
    DAG_node* cur_node = node_array[i]->next;
    while (cur_node != NULL) {
      children_set.insert(cur_node->index);
      cur_node = cur_node->next;
    }
    return children_set;
  }

  set<int> get_parents_set(int i) {
    set<int> parents_set;
    DAG_node* cur_node = node_array[i]->prev;
    while (cur_node != NULL) {
      parents_set.insert(cur_node->index);
      cur_node = cur_node->prev;
    }
    return parents_set;
  }

private:
  int n;
  DAG_node** node_array;
};

class ListNode {
public:
  int row_index;
  int col_index;
  ListNode* next;
  ListNode() {
    next = NULL;
  }
};

class List {
private:
  ListNode* first;
  int size;
public:
  List() {
    size = 0;
    first = NULL;
  }
  ~List() {
    while (first != NULL) {
      ListNode* to_delete_node = first;
      first = to_delete_node->next;
      delete to_delete_node;
    }
  }
  void push(int col_index, int row_index) {
    ListNode* newNode = new ListNode();
    newNode->col_index = col_index;
    newNode->row_index = row_index;
    newNode->next = first;
    first = newNode;
    size++;
  }

  void pop(int& col_index, int& row_index) {
    col_index = first->col_index;
    row_index = first->row_index;
    ListNode* to_delete_node = first;
    first = to_delete_node->next;
    delete to_delete_node;
    size--;
  }

  int get_size() {
    return size;
  }
};


class orderedBlock {
public:
  orderedBlock(int size, double* y = NULL, double* w = NULL) {
    n = size;
    xl = new double[n];
    calibrated_xl = new double[n];
    weights = new double[n];
    if (w != NULL) {
      weighted = true;
    }
    else {
      weighted = false;
    }
    if (y != NULL) {
      for (int i = 0;i < n;i++) {
        xl[i] = y[i];
        calibrated_xl[i] = y[i];
      }
      if (weighted) {
        for (int i = 0;i < n;i++) {
          weights[i] = w[i];
        }
      }
    }

    str_DAG = DAG();
    str_DAG.initialize(n);
  }

  ~orderedBlock() {
    delete[]xl;
    delete[]calibrated_xl;
  }

  void load_chain_isotonic_str() {
    m = n - 1;
    E = new int* [m];
    for (int i = 0;i < m;i++) {
      E[i] = new int[2];
    }
    int E_count = 0;

    for (int i = 0;i < m;i++) {
      str_DAG.connect(i, i + 1);
      E[E_count][0] = i;
      E[E_count][1] = i + 1;
      E_count++;
    }
  }

  void load_isotonic_str_via_mats(int** E_mat, int m_input) {
    m = m_input;
    E = new int* [m];
    for (int i = 0;i < m;i++) {
      E[i] = new int[2];
    }

    for (int i = 0;i < m;i++) {
      str_DAG.connect(E_mat[i][0], E_mat[i][1]);
      E[i][0] = E_mat[i][0];
      E[i][1] = E_mat[i][1];
      //cout << E_mat[i][0] << " " << E_mat[i][1] << endl;
      //cout << E[i][0] << " " << E[i][1] << endl;
    }
  }

  void load_tree_isotonic_str() {
    m = n - 1;
    E = new int* [m];
    for (int i = 0;i < m;i++) {
      E[i] = new int[2];
    }

    int h = floor(log2(n + 1));
    int count = 0;
    for (int i = 0; i < pow(2, h - 1) - 1;i++) {
      for (int ii = 2 * i + 1; ii < 2 * i + 3; ii++) {
        E[count][0] = i;
        E[count][1] = ii;
        str_DAG.connect(i, ii);
        count++;
      }
    }
    int temp_i;
    for (int i = pow(2, h) - 1;i < n;i++) {
      temp_i = int(floor(double(i) / 2 + 0.5)) - 1;
      E[count][0] = temp_i;
      E[count][1] = i;
      str_DAG.connect(temp_i, i);
      count++;
    }

  }

  void load_2D_isotonic_str() {
    int nl = floor(sqrt(n));
    m = nl * nl - 2 * nl + n;
    E = new int* [m];
    for (int i = 0;i < m;i++) {
      E[i] = new int[2];
    }
    int E_count = 0;

    for (int i = 0;i < nl - 1;i++) {
      for (int j = 0;j < nl - 1;j++) {
        str_DAG.connect(j + nl * i, (j + 1) + nl * i);
        E[E_count][0] = j + nl * i;
        E[E_count][1] = (j + 1) + nl * i;
        E_count++;
        str_DAG.connect(j + nl * i, j + nl * (i + 1));
        E[E_count][0] = j + nl * i;
        E[E_count][1] = j + nl * (i + 1);
        E_count++;
      }
      str_DAG.connect(nl - 1 + nl * i, nl - 1 + nl * (i + 1));
      E[E_count][0] = nl - 1 + nl * i;
      E[E_count][1] = nl - 1 + nl * (i + 1);
      E_count++;
    }
    for (int j = nl * (nl - 1);j < n - 1;j++) {
      str_DAG.connect(j, j + 1);
      E[E_count][0] = j;
      E[E_count][1] = j + 1;
      E_count++;
    }
  }

  void print_E() {
    for (int i = 0;i < m;i++) {
      cout << E[i][0] << " " << E[i][1] << endl;
    }
  }

  void solve_by_SBM(bool remember_adj_gra = false) {
    outer_loop = 0;
    inner_loop = 0;
    for (int i = 0;i < n;i++) {
      calibrated_xl[i] = xl[i];
    }
    Adjacency_list* Adjacency_obj = new Adjacency_list(n);
    double* Ax = compute_Ax();
    int min_pos = whichMin(Ax, m);
    double min_value = Ax[min_pos];
    int i, j;
    double x_bar;
    double* local_lambda;
    set<int> block_set;
    while (min_value < -1e-9) {
      outer_loop++;
      i = E[min_pos][0];
      j = E[min_pos][1];
      if (Adjacency_obj->isEmpty(i) && Adjacency_obj->isEmpty(j)) {
        Adjacency_obj->connect(i, j);
        //cout << "Connecting " << i << "-" << j << endl;
        if (weighted) {
          x_bar = (xl[i] * weights[i] + xl[j] * weights[j]) / (weights[i] + weights[j]);
        }
        else {
          x_bar = 0.5 * (xl[i] + xl[j]);
        }
        calibrated_xl[i] = x_bar;
        calibrated_xl[j] = x_bar;
      }
      else {
        Adjacency_obj->connect(i, j);
        //cout << "Connecting " << i << "-" << j << endl;
        block_set = Adjacency_obj->compute_subgraph(i); // get all the index inside this block
        x_bar = get_x_bar(block_set);
        local_lambda = solve_local_KKT(block_set, x_bar, El, Adjacency_obj);
        for (set<int>::iterator it = block_set.begin();it != block_set.end();it++) {
          calibrated_xl[*it] = x_bar;
          // cout << "x[" << *it << "] = " << x_bar << endl;
        }
        if (get_min_value(local_lambda, block_set.size() - 1) < -1e-12) {
          // go to inner loop
          set<int> G_bar_index_set;
          for (int i = 0;i < block_set.size() - 1;i++) {
            G_bar_index_set.insert(i);
          }
          current_nbo = block_set.size();
          solve_inner_loop(G_bar_index_set, local_lambda, Adjacency_obj);
        }
        for (int i = 0;i < block_set.size() - 1;i++) {
          delete[]El[i];
        }
        delete[]El;
        delete[]local_lambda;
      }
      Compute_min_diff(min_value, min_pos, Ax);
    }
    if (remember_adj_gra) {
      //Adjacency_obj.print();
      if (warm_start_obj != NULL) {
        delete warm_start_obj;
      }
      warm_start_obj = Adjacency_obj;
    }
    else {
      delete Adjacency_obj;
    }
  }

  void solve_inner_loop(set<int> &inequality_indexset, double *lambda, Adjacency_list* Adjacency_obj) {
    set<int> current_equality_indexset;
    set<int> current_inequality_indexset;
    // initialization
    for (set<int>::iterator it = inequality_indexset.begin(); it != inequality_indexset.end();it++) {
      if (calibrated_xl[El[*it][0]] != calibrated_xl[El[*it][1]]) {
        current_inequality_indexset.insert(*it);
      }
      else {
        current_equality_indexset.insert(*it);
      }
    }
    bool local_kkt_solved = true;
    double min_lambda = 0;
    int min_lambda_pos = 0;
    int** El_i; //local_str_info
    int** El_j; //local_str_info
    for (set<int>::iterator it = inequality_indexset.begin(); it != inequality_indexset.end();it++) {
      if (lambda[*it] < min_lambda) {
        local_kkt_solved = false;
        min_lambda = lambda[*it];
        min_lambda_pos = *it;
      }
    }
    while (!local_kkt_solved) {
      inner_loop++;
      int to_delete_i = El[min_lambda_pos][0];
      int to_delete_j = El[min_lambda_pos][1];
      Adjacency_obj->disconnect(to_delete_i, to_delete_j);
      current_inequality_indexset.insert(min_lambda_pos);
      current_equality_indexset.erase(min_lambda_pos);

      // cout << "Disconnecting " << to_delete_i << "-" << to_delete_j << endl;
      lambda[min_lambda_pos] = 0;
      set<int> block_set_i = Adjacency_obj->compute_subgraph(to_delete_i);
      set<int> block_set_j = Adjacency_obj->compute_subgraph(to_delete_j);

      // re-optimize individual blocks, update x and lambda
      double x_bar_i = get_x_bar(block_set_i);
      double x_bar_j = get_x_bar(block_set_j);
      for (set<int>::iterator it = block_set_i.begin();it != block_set_i.end();it++) {
        calibrated_xl[*it] = x_bar_i;
      }
      for (set<int>::iterator it = block_set_j.begin();it != block_set_j.end();it++) {
        calibrated_xl[*it] = x_bar_j;
      }
      if (block_set_i.size() != 1) {
        double* local_lambda_i = solve_local_KKT(block_set_i, x_bar_i, El_i, Adjacency_obj);
        updata_local_lamdba(lambda, El, local_lambda_i, El_i, block_set_i.size() - 1);
      }
      if (block_set_j.size() != 1) {
        double* local_lambda_j = solve_local_KKT(block_set_j, x_bar_j, El_j, Adjacency_obj);
        updata_local_lamdba(lambda, El, local_lambda_j, El_j, block_set_j.size() - 1);
      }

      // repair primal feasibility if necessary
      int min_i = 0;
      int min_j = 0;
      bool is_still_primal_feasible = is_primal_feasible(El, current_nbo - 1, min_i, min_j);

      while (!is_still_primal_feasible) {
        Adjacency_obj->connect(min_i, min_j);
        //cout << "re-connect " << min_i << "-" << min_j << endl;
        set<int> subcurrent_set = Adjacency_obj->compute_subgraph(min_i);
        int** l_El = new int* [subcurrent_set.size() - 1];
        double l_x_bar = get_x_bar(subcurrent_set);
        double* l_local_lambda = solve_local_KKT(subcurrent_set, l_x_bar, l_El, Adjacency_obj);
        updata_local_lamdba(lambda, El, l_local_lambda, l_El, subcurrent_set.size() - 1);
        for (set<int>::iterator it = subcurrent_set.begin();it != subcurrent_set.end();it++) {
          calibrated_xl[*it] = l_x_bar;
        }
        for (int i = 0;i < subcurrent_set.size() - 1;i++) {
          delete[]l_El[i];
        }
        delete[]l_El;
        delete[]l_local_lambda;
        is_still_primal_feasible = is_primal_feasible(El, current_nbo - 1, min_i, min_j);
      }

      // Whether go to recursion
      min_lambda = 0;
      for (set<int>::iterator it = current_inequality_indexset.begin(); it != current_inequality_indexset.end();it++) {
        if (lambda[*it] < min_lambda) {
          min_lambda = lambda[*it];
          min_lambda_pos = *it;
        }
      }
      if (min_lambda < 0) {
        solve_inner_loop(current_inequality_indexset, lambda, Adjacency_obj);
      }

      local_kkt_solved = true;
      min_lambda = 0;
      for (set<int>::iterator it = inequality_indexset.begin(); it != inequality_indexset.end();it++) {
        if (lambda[*it] < min_lambda) {
          local_kkt_solved = false;
          min_lambda = lambda[*it];
          min_lambda_pos = *it;
        }
      }
    }
  }


  int get_inner_loop_num() {
    return(inner_loop);
  }
  int get_outer_loop_num() {
    return(outer_loop);
  }

  void print_xl() {
    for (int j = 0;j < n;j++) {
      cout << calibrated_xl[j] << " ";
    }
    cout << endl;
  }


  void get_x_calibrated(double* x_to_write) {
    for (int i = 0;i < n;i++) {
      x_to_write[i] = calibrated_xl[i];
    }
  }


private:
  int n; // number of node
  int m; // number of constraints or edges
  double* xl; // data of every nodes
  double* calibrated_xl; // calibrated xl
  double* weights; // weights
  bool weighted;
  DAG str_DAG;
  Adjacency_list* warm_start_obj;

  int outer_loop;
  int inner_loop;
  int current_nbo = 0;

  int** E; // constraints input matrix
  int** El; //local_str_info
  double* compute_Ax() {
    double* Ax = new double[m];
    for (int i = 0;i < m;i++) {
      Ax[i] = -calibrated_xl[E[i][0]] + calibrated_xl[E[i][1]];
    }
    return Ax;
  }

  int whichMin(double* x, int size) {
    int min_pos;
    min_pos = 0;
    for (int i = 1;i < size;i++) {
      if (x[i] < x[min_pos]) {
        min_pos = i;
      }
    }
    return min_pos;
  }
  double get_min_value(double* x, int size) {
    double min_value = x[0];
    for (int i = 1;i < size;i++) {
      if (x[i] < min_value) {
        min_value = x[i];
      }
    }
    return min_value;
  }
  double get_x_bar(set<int> inactive_set) {
    double x_bar = 0.0;
    if (weighted) {
      double w_sum = 0.0;
      for (set<int>::iterator it = inactive_set.begin();it != inactive_set.end();it++) {
        x_bar = x_bar + weights[*it] * xl[*it];
        w_sum = w_sum + weights[*it];
      }
      x_bar = x_bar / w_sum;
    }
    else {
      for (set<int>::iterator it = inactive_set.begin();it != inactive_set.end();it++) {
        x_bar = x_bar + xl[*it];
      }
      x_bar = x_bar / inactive_set.size();
    }
    return x_bar;
  }



  void Compute_min_diff(double& min_value, int& min_pos, double*& Ax) {
    min_value = 0.0;
    for (int i = 0;i < m;i++) {
      Ax[i] = -calibrated_xl[E[i][0]] + calibrated_xl[E[i][1]];
      if (Ax[i] < min_value) {
        min_value = Ax[i];
        min_pos = i;
      }
    }
  }

  set<int> get_violator_set(double* x) {
    set<int> violator_set;
    for (int i = 0;i < m;i++) {
      if (-x[E[i][0]] + x[E[i][1]] < -1e-9) {
        violator_set.insert(i);
      }
    }
    return violator_set;
  }

  double partial_fi(int i, double x_i) {
    if (weighted) {
      return(weights[i] * (x_i - xl[i]));
    }
    else {
      return(x_i - xl[i]);
    }
  }

  void updata_local_lamdba(double*& big_local_lambda, int** El_big, double*& small_local_lambda, int** El_small, int small_n) {
    int countEl;
    for (int ii = 0; ii < small_n;ii++) {
      countEl = 0;
      while (!(El_big[countEl][0] == El_small[ii][0] && El_big[countEl][1] == El_small[ii][1]))
      {
        countEl++;
      }
      big_local_lambda[countEl] = small_local_lambda[ii];
    }
  }

  double* solve_local_KKT(set<int>& inactive_set, double& x_bar, int**& El, Adjacency_list* Adjacency_obj) {
    int nl = inactive_set.size() - 1;
    double** Al = new double* [nl];
    double* bl = new double[nl];
    El = new int* [nl];
    for (int i = 0;i < nl;i++) {
      El[i] = new int[2];
      Al[i] = new double[nl];
      for (int j = 0;j < nl;j++) {
        Al[i][j] = 0;
      }
      bl[i] = 0;
      El[i][0] = 0;
      El[i][1] = 0;
    }
    int count_El = 0;
    int* temp_array = new int[nl + 1];
    for (set<int>::iterator it = inactive_set.begin();it != inactive_set.end();it++) {
      temp_array[count_El] = int(*it);
      count_El++;
    }
    int* row_abs_sum = new int[nl];
    int* unique_col_index = new int[nl];
    for (int i = 0;i < nl;i++) {
      row_abs_sum[i] = 0;
    }
    count_El = 0;
    int ii, jj;
    for (int i = 0;i < inactive_set.size();i++) {
      for (int j = 0; j < inactive_set.size();j++) {
        if (i != j) {
          ii = temp_array[i];
          jj = temp_array[j];
          if (str_DAG.is_connected(ii, jj) && Adjacency_obj->is_directly_connected(ii, jj)) {
            El[count_El][0] = temp_array[i];
            El[count_El][1] = temp_array[j];
            if (i < nl) {
              Al[i][count_El] = 1;
              row_abs_sum[i] = row_abs_sum[i] + 1;
              unique_col_index[i] = count_El;
            }
            if (j < nl) {
              Al[j][count_El] = -1;
              row_abs_sum[j] = row_abs_sum[j] + 1;
              unique_col_index[j] = count_El;
            }
            //cout << El[count_El][0] << " " << El[count_El][1] << endl;
            count_El++;
          }
        }
      }
    }
    delete[]temp_array;
    int count_eq = 0;
    for (set<int>::iterator it = inactive_set.begin();it != inactive_set.end();it++) {
      if (count_eq < nl) {
        bl[count_eq] = -partial_fi(*it, x_bar);
        count_eq++;
      }
    }
    double* local_lambda = solve_linear_system(Al, bl, nl);

    for (int i = 0;i < nl;i++) {
      delete[]Al[i];
    }
    delete[]Al;
    delete[]bl;
    delete[]unique_col_index;

    return local_lambda;
  }

  double* solve_linear_system(double**& Al, double*& bl, int& nl) {
    double** Aug = new double* [nl];
    for (int i = 0;i < nl;i++) {
      Aug[i] = new double[nl + 1];
      for (int j = 0;j < nl;j++) {
        Aug[i][j] = Al[i][j];
      }
      Aug[i][nl] = bl[i];
    }
    int k;
    double max, temp, l, s;
    for (int i1 = 0; i1 < nl - 1; i1++)
    {
      max = fabs(Aug[i1][i1]);
      k = i1;
      for (int i = i1; i < nl; i++)
      {
        if (max < fabs(Aug[i][i1]))
        {
          max = fabs(Aug[i][i1]);
          k = i;
        }
      }

      for (int j = i1; j < nl + 1; j++)
      {
        temp = Aug[i1][j];
        Aug[i1][j] = Aug[k][j];
        Aug[k][j] = temp;
      }

      for (k = i1 + 1; k < nl; k++)
      {
        l = -Aug[k][i1] / Aug[i1][i1];
        for (int j = i1; j < nl + 1; j++)
          Aug[k][j] = Aug[k][j] + l * Aug[i1][j];
      }
    }

    double* x_star = new double[nl];
    x_star[nl - 1] = Aug[nl - 1][nl] / Aug[nl - 1][nl - 1];

    for (int i = nl - 2; i >= 0; i = i - 1)
    {
      s = 0;
      for (int j = i + 1; j < nl; j++)
        s = s + Aug[i][j] * x_star[j];
      x_star[i] = (Aug[i][nl] - s) / Aug[i][i];
    }

    for (int i = 0;i < nl;i++) {
      delete[]Aug[i];
    }
    delete[]Aug;

    return x_star;
  }

  bool is_primal_feasible(set<int> inactive_set) {
    bool is_feasible = true;
    for (set<int>::iterator it = inactive_set.begin();it != inactive_set.end();it++) {
      for (set<int>::iterator it2 = inactive_set.begin();it2 != inactive_set.end();it2++) {
        if (str_DAG.is_connected(*it, *it2) && calibrated_xl[*it] - calibrated_xl[*it2] > 10e-9) {
          is_feasible = false;
        }
      }
    }
    return is_feasible;
  }

  bool is_primal_feasible(int**& El, int nl, int& min_i, int& min_j) {
    double min_value = 0.0;
    for (int i = 0; i < nl;i++) {
      if (-calibrated_xl[El[i][0]] + calibrated_xl[El[i][1]] < min_value) {
        min_value = -calibrated_xl[El[i][0]] + calibrated_xl[El[i][1]];
        min_i = El[i][0];
        min_j = El[i][1];
      }
    }
    return (min_value >= 0) ? true : false;
  }
};


// [[Rcpp::export]]
NumericVector solve_ordered_chain(NumericVector y_input, NumericVector w_input) {
  int n = y_input.size();
  double* y = new double[n];
  double* w = new double[n];
  for(int i = 0; i < n; i++) {
    y[i] = y_input[i];
    w[i] = w_input[i];
  }
  orderedBlock* BT = new orderedBlock(n, y, w);
  BT->load_chain_isotonic_str();
  BT->solve_by_SBM();

  double* x_ordered = new double[n];
  BT->get_x_calibrated(x_ordered);
  NumericVector x_res(n);
  for(int i = 0; i < n; i++) {
    x_res[i] = x_ordered[i];
  }
  return(x_res);
}


// [[Rcpp::export]]
NumericVector solve_ordered_binary_tree(NumericVector y_input, NumericVector w_input) {
  int n = y_input.size();
  double* y = new double[n];
  double* w = new double[n];
  for(int i = 0; i < n; i++) {
    y[i] = y_input[i];
    w[i] = w_input[i];
  }
  orderedBlock* BT = new orderedBlock(n, y, w);
  BT->load_tree_isotonic_str();
  BT->solve_by_SBM();

  double* x_ordered = new double[n];
  BT->get_x_calibrated(x_ordered);
  NumericVector x_res(n);
  for(int i = 0; i < n; i++) {
    x_res[i] = x_ordered[i];
  }
  return(x_res);
}

// [[Rcpp::export]]
NumericVector solve_ordered_2d_grid(NumericVector y_input, NumericVector w_input) {
  int n = y_input.size();
  double* y = new double[n];
  double* w = new double[n];
  for(int i = 0; i < n; i++) {
    y[i] = y_input[i];
    w[i] = w_input[i];
  }
  orderedBlock* BT = new orderedBlock(n, y, w);
  BT->load_2D_isotonic_str();
  BT->solve_by_SBM();

  double* x_ordered = new double[n];
  BT->get_x_calibrated(x_ordered);
  NumericVector x_res(n);
  for(int i = 0; i < n; i++) {
    x_res[i] = x_ordered[i];
  }
  return(x_res);
}

// [[Rcpp::export]]
NumericVector solve_ordered_arbitrary_DAG(NumericVector y_input, NumericVector w_input, NumericMatrix E_mat_input) {
  int n = y_input.size();
  double* y = new double[n];
  double* w = new double[n];
  for(int i = 0; i < n; i++) {
    y[i] = y_input[i];
    w[i] = w_input[i];
  }
  orderedBlock* BT = new orderedBlock(n, y, w);

  int m = E_mat_input.nrow();
  int** E_mat = new int*[m];
  for(int i=0;i<m;i++) {
    E_mat[i] = new int[2];
    E_mat[i][0] = int(E_mat_input(i,0));
    E_mat[i][1] = int(E_mat_input(i,1));
    //cout << E_mat[i][0] << " " << E_mat[i][1] << endl;
  }
  //cout << m << endl;

  BT->load_isotonic_str_via_mats(E_mat, m);
  //BT->print_E();
  BT->solve_by_SBM();

  double* x_ordered = new double[n];
  BT->get_x_calibrated(x_ordered);
  NumericVector x_res(n);
  for(int i = 0; i < n; i++) {
    x_res[i] = x_ordered[i];
  }
  return(x_res);
}






