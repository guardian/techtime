# encoding: UTF-8

require_relative "leaderboard-core.rb"

pointsToLeaderboard(getPoints()).each{|p|
    name = p["name"]
    score = p["score"]
    puts "#{name.ljust(20)}: #{"%9.6f" % score}"
}
