# encoding: UTF-8

require_relative "LeaderboardLibrary.rb"

HALF_YEAR_IN_DAYS = 182.62
POINTS_ROOT_FOLDERPATH = "#{File.dirname(__FILE__)}/points-files"

LeaderboardLibrary::pointsToLeaderboard(LeaderboardLibrary::getPoints(POINTS_ROOT_FOLDERPATH), HALF_YEAR_IN_DAYS).each{|p|
    name = p["name"]
    score = p["score"]
    puts "#{name.ljust(20)}: #{"%9.6f" % score}"
}
