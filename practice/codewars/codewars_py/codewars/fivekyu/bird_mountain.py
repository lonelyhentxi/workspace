from typing import List, Optional, Sequence

mask_actions = [(1, 0), (-1, 0), (0, 1), (0, -1)]


def peak_height(raw_mountain: List[str]):
    height = len(raw_mountain)
    width = len(raw_mountain[0])

    def get_peak(mountain: List[Sequence[str]], y: int, x: int):
        if y < 0 or y >= height or x < 0 or x >= width:
            return ' '
        else:
            return mountain[y][x]

    def parse_peak(area: str) -> Optional[int]:
        if area == '^':
            return None
        if area == ' ':
            return 0
        else:
            return int(area)

    def mask(mountain: List[Sequence[str]], current_y: int, current_x: int, altitude: int) -> bool:
        current = parse_peak(mountain[current_y][current_x])
        if current is not None:
            return False
        for y, x in mask_actions:
            peak = get_peak(mountain, current_y + y, current_x + x)
            target = parse_peak(peak)
            if target is not None and target < altitude:
                return True
        return False

    current_pass: List[List[str]] = []
    prev_pass = raw_mountain
    current_altitude = 1
    while True:
        count = 0
        for i in range(0, height):
            current_pass.append([])
            for j in range(0, width):
                if mask(prev_pass, i, j, current_altitude):
                    current_pass[i].append(str(current_altitude))
                    count += 1
                else:
                    current_pass[i].append(prev_pass[i][j])
        if count == 0:
            break
        else:
            current_altitude += 1
        prev_pass = current_pass
        current_pass = []
    return current_altitude - 1
