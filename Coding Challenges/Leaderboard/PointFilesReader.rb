# encoding: UTF-8

require 'json'
require 'date'
require 'find'

=begin

Point Files:

    The root folderpath is meant to be a fs location under which every text file (suffix: .txt) is 
    going to be interpreted as a points file

    Points files contains enries of the form

    ```
    Luke Skywalker; 2018-06-29 17:30:00 +0100; 1
    ```

    Lines starting with # are ignored.

=end

class PointFilesReader

    # PointFilesReader::pointsFilepaths(rootfolderpath)
    def self.pointsFilepaths(rootfolderpath)
        files = []
        Find.find(rootfolderpath) do |path|
            next if path[-4,4] != ".txt"
            files << path
        end
        files
    end

    # PointFilesReader::getDTYLeaderboardPoints(rootfolderpath): DTYLeaderboardPoints
    def self.getDTYLeaderboardPoints(rootfolderpath)
        PointFilesReader::pointsFilepaths(rootfolderpath).map{|filepath|
            IO.read(filepath)
                .lines
                .map{|line| line.strip }
                .select{|line| line.size>0 }
                .select{|line| !line.start_with?("#") }
                .map{|line| Hash[["name", "time", "value"].zip(line.split(";").map{|i| i.strip})] }
                .map{|item| 
                    item["value"] = item["value"].to_f 
                    item
                }
        }.flatten
    end

end

    
