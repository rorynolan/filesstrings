#ifndef DETRENDR_RBOXES_
#define DETRENDR_RBOXES_

#include <vector>
#include <cstdint>
#include <random>

template <class IntType, class IntVec, class NumVec /*float or int type*/>
IntVec rfromboxes(IntType n, IntVec& balls, NumVec& weights, int seed) {
  // beware that this function modifies `balls` and `weights`
  std::size_t balls_sz = balls.size();
  for (std::size_t i = 0; i != balls_sz; ++i) {
    if (balls[i] == 0)
      weights[i] = 0;
  }
  std::default_random_engine generator(seed);
  IntVec out(balls_sz);
  typedef std::discrete_distribution<std::size_t> ddIT;
  ddIT distribution(weights.begin(), weights.end());
  for (IntType i = 0; i != n; ++i) {
    std::size_t draw = distribution(generator);
    out[draw]++;
    balls[draw]--;
    if (balls[draw] == 0) {
      weights[draw] = 0;
      distribution = ddIT(weights.begin(), weights.end());
    }
  }
  return out;
}

template <class IntType, class IntVec, class NumVec /*float or int type*/>
IntVec rtoboxes(IntType n, IntType boxes,
                NumVec& weights, IntVec& capacities, int seed) {
  // beware that this function modifies `weights` and `capacities`
  std::size_t cap_sz = capacities.size();
  uintmax_t intmax_t_max = std::numeric_limits<uintmax_t>::max();
  for (std::size_t i = 0; i != cap_sz; ++i) {
    if (capacities[i] == -1)
      capacities[i] = intmax_t_max;
  }
  for (std::size_t i = 0; i != cap_sz; ++i) {
    if (capacities[i] == 0)
      weights[i] = 0;
  }
  std::default_random_engine generator(seed);
  IntVec out(boxes);
  typedef std::discrete_distribution<uintmax_t> ddIT;
  ddIT distribution(weights.begin(), weights.end());
  for (std::size_t i = 0; i != n; ++i) {
    std::size_t draw = distribution(generator);
    out[draw]++;
    if (out[draw] == capacities[draw]) {
      weights[draw] = 0;
      distribution = ddIT(weights.begin(), weights.end());
    }
  }
  return out;
}


#endif  // DETRENDR_RBOXES_
