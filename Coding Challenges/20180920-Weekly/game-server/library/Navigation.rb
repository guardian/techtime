
# encoding: UTF-8

class Navigation

    # Navigation::distanceBetweenTwoMapPoints(point1, point2)
    def self.distanceBetweenTwoMapPoints(point1, point2)
        dx = point1["coordinates"][0] - point2["coordinates"][0]
        dy = point1["coordinates"][1] - point2["coordinates"][1]
        Math.sqrt( (dx**2) + (dy**2) )
    end

    # Navigation::jumpEnergyCost(sourceMapPoint, targetMapPoint, shipType)
    def self.jumpEnergyCost(sourceMapPoint, targetMapPoint, shipType)
        # For the moment we are ignoring the ship type
        1
    end

end

