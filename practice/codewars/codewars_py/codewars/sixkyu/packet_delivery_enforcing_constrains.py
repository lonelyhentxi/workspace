class DimensionsOutOfBoundError(Exception):
    def __init__(self, *args):
        super().__init__(*args)

    def __str__(self):
        return super().__str__()


class Package:
    def __init__(self, length: float, width: float, height: float, weight: float):
        self.length = length
        self.height = height
        self.weight = weight
        self.width = width

    @property
    def length(self):
        return self._length

    @length.setter
    def length(self, value: float):
        if not 0 < value <= 350:
            raise DimensionsOutOfBoundError(f"Package length=={value} out of bounds, should be: 0 < length <= 350")
        else:
            self._length = value

    @property
    def width(self):
        return self._width

    @width.setter
    def width(self, value: float):
        if not 0 < value <= 300:
            raise DimensionsOutOfBoundError(f"Package width=={value} out of bounds, should be: 0 < width <= 300")
        else:
            self._width = value

    @property
    def height(self):
        return self._height

    @height.setter
    def height(self, value: float):
        if not 0 < value <= 150:
            raise DimensionsOutOfBoundError(f"Package height=={value} out of bounds, should be: 0 < height <= 150")
        else:
            self._height = value

    @property
    def weight(self):
        return self._weight

    @weight.setter
    def weight(self, value: float):
        if not 0 < value <= 40:
            raise DimensionsOutOfBoundError(f"Package weight=={value} out of bounds, should be: 0 < weight <= 40")
        else:
            self._weight = value

    @property
    def volume(self):
        return self.length * self.width * self.height
