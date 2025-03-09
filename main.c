#include "mi.h"

lifetime <region p>
void test_heap(void mi(p)* p_out) {
  region q;
  mi_heap_t mi(q)* heap = mic_heap_new<q>();
  void mi(q)* p1 = mic_heap_malloc<q>(heap,32);
  void mi(q)* p2 = mic_heap_malloc<q>(heap,48);
  //mic_free<p>(p_out);
  mic_free<q>(p1); mic_free<q>(p2);
}
lifetime void test_heap(void mi(0)* p_out);
int main() {
  
  {
    region p;
    mi_heap_t mi(p)* heap = mic_heap_new<p>();
    test_heap<p>(mic_heap_malloc<p>(heap, 32));
  }
  mi_collect(true);
  mi_stats_print(NULL);

  return 0;
}