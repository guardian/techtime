# encoding: UTF-8

require_relative "PointFilesReader.rb"
require_relative "Leaderboard.rb"

POINTS_ROOT_FOLDERPATH = "#{File.dirname(__FILE__)}/points-files"
HALF_YEAR_IN_DAYS = 182.62

points = PointFilesReader::getDTYLeaderboardPoints(POINTS_ROOT_FOLDERPATH)
userscores = Leaderboard::convertDTYLeaderboardPointsToDTYLeaderboardFinalUserScores(points, HALF_YEAR_IN_DAYS)

userscores.each{|p|
    name = p["name"]
    score = p["score"]
    puts "#{name.ljust(20)}: #{"%9.6f" % score}"
}
