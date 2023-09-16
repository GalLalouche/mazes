package mazes.data

enum Direction:
  case North, South, East, West

  def opposite: Direction = this match {
    case North => South
    case South => North
    case West => East
    case East => West
  }
