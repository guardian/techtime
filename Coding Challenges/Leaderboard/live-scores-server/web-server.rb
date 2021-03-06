
# encoding: UTF-8

# ------------------------------------------------------------
# utils

require 'sinatra'
# http://www.sinatrarb.com/intro.html

require 'securerandom'
# SecureRandom.hex    #=> "eb693ec8252cd630102fd0d0fb7c3485"
# SecureRandom.hex(2) #=> "eb69"
# SecureRandom.uuid   #=> "2d931510-d99f-494a-8c67-87feb05e1594"

require 'json'

require_relative "../PointFilesReader.rb"
require_relative "../Leaderboard.rb"

# --  --------------------------------------------------

set :port, 13999
#set :public_folder, "path/to/www"

POINTS_ROOT_FOLDERPATH = "#{File.dirname(__FILE__)}/../points-files"
HALF_YEAR_IN_DAYS = 182.62

# -- --------------------------------------------------
# Route

not_found do
  '404'
end

get '/' do
    content_type 'text/plain'
    "/leaderboard : for the live leaderboard\n"
end

get '/leaderboard' do
    content_type 'text/plain'
    points = PointFilesReader::getDTYLeaderboardPoints(POINTS_ROOT_FOLDERPATH)
    userscores = Leaderboard::convertDTYLeaderboardPointsToDTYLeaderboardFinalUserScores(points, HALF_YEAR_IN_DAYS)
    userscores
        .map{|p| "#{p["name"].ljust(20)}: #{"%9.6f" % p["score"]}" }
        .join("\n") + "\n"
end
