
# encoding: UTF-8

class MapUtils

    # MapUtils::getCurrentMap()
    def self.getCurrentMap()
        folderpath = nil
        $mapInitMutex.synchronize {
            folderpath = GameLibrary::ensureGameFolderSetUpForThisHour()
        }
        GameLibrary::getMapAtHourFolderpath(folderpath)
    end

    # MapUtils::getPointForlabelAtMapOrNull(label, map)
    def self.getPointForlabelAtMapOrNull(label, map)
        map["points"].each{|point|
            return point if point["label"]==label
        }
        nil
    end

    # MapUtils::distanceBetweenTwoMapPoints(point1, point2)
    def self.distanceBetweenTwoMapPoints(point1, point2)
        dx = point1["coordinates"][0] - point2["coordinates"][0]
        dy = point1["coordinates"][1] - point2["coordinates"][1]
        Math.sqrt( (dx**2) + (dy**2) )
    end

end

