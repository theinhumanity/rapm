from pbpstats.client import Client
from pbpstats.resources.enhanced_pbp import FieldGoal

settings = {
    "Boxscore": {"source": "web", "data_provider": "live"},
    "Possessions": {"source": "web", "data_provider": "live"},
}

client = Client(settings)
game = client.Game("0021900001")

for possession in game.possessions.items:
    for event in possession.events:
        print(event, event.current_players)