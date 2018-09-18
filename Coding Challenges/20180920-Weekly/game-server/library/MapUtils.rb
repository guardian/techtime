
# encoding: UTF-8

class MapUtils

    # MapUtils::getCurrentMap()
    def self.getCurrentMap()
        folderpath = GameLibrary::ensureGameFolderSetUpForThisHour()
        GameLibrary::getMapAtHourFolderpath(folderpath)
    end

    # MapUtils::getPointForlabelAtMapOrNull(label, map)
    def self.getPointForlabelAtMapOrNull(label, map)
        map["points"].each{|point|
            return point if point["label"]==label
        }
        nil
    end

end

