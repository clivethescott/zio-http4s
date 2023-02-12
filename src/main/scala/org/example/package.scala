package org

import zio.Has

package object example {
  type Greeter = Has[Greeter.Service]
}
