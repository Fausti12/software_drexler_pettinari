package org.udesa.unoapplication.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.udesa.unoapplication.model.Card;
import org.udesa.unoapplication.model.Match;
import org.udesa.unoapplication.model.Player;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
public class service {
    @Autowired private Dealer dealer;
    private Map<UUID, Match> sessions = new HashMap<UUID, Match>();


    public UUID newMatch(List<String > players){
        UUID newKey = UUID.randomUUID();
        sessions.put(newKey, Match.fullMatch(dealer.fullDeck(), players));
        return newKey;
    }

    public List<Card> playerHand(UUID matchId){
        return sessions.get(matchId).playerHand();
    }

    public void play(UUID matchId, String player, Card card){
        sessions.get(matchId).play(player, card);
    }

    public Card activeCard(UUID matchId){
        return sessions.get(matchId).activeCard();
    }

    public void drawCard(UUID matchId, String player){
        sessions.get(matchId).drawCard(player);
    }


}
