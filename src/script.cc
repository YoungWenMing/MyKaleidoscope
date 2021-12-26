#include "src/script.h"

#include <algorithm>
#include "src/global.h"

namespace Kaleidoscope {

Location Script::get_location(int code_pos) const {
  auto upper_bound = std::upper_bound(line_ends_.begin(), line_ends_.end(), code_pos);
  DCHECK(upper_bound != line_ends_.end());
  return Location(upper_bound - line_ends_.begin(),
                  code_pos - *(upper_bound - 1) - 1);
}

} // Kaleidoscope 