# encoding: UTF-8

require_relative "Leaderboard.rb"

POINTS_ROOT_FOLDERPATH = "#{File.dirname(__FILE__)}/points-files"
HALF_YEAR_IN_DAYS = 182.62

Leaderboard::getStructureNX2010(POINTS_ROOT_FOLDERPATH, HALF_YEAR_IN_DAYS)
.each{|p|
    name = p["name"]
    score = p["score"]
    puts "#{name.ljust(20)}: #{"%9.6f" % score}"
}
