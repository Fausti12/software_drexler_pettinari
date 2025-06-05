package org.udesa.unoapplication.service;

import org.springframework.stereotype.Service;
import org.udesa.unoapplication.model.*;

import java.util.*;

@Service
public class UnoService {

    private final Map<UUID, Match> matches = new HashMap<>();

    public UUID createNewMatch(List<String> players) {
        List<Card> deck = fullDeck();
        Collections.shuffle(deck);
        Match match = Match.fullMatch(deck, players); //repartir 7 cartas
        UUID id = UUID.randomUUID();
        matches.put(id, match);
        return id;
    }

    public void play(UUID matchId, String player, Card card) {
        getMatch(matchId).play(player, card);
    }

    public void drawCard(UUID matchId, String player) {
        getMatch(matchId).drawCard(player);
    }

    public Card visibleCard(UUID matchId) {
        return getMatch(matchId).activeCard();
    }

    public List<Card> handOfCurrentPlayer(UUID matchId) {
        return getMatch(matchId).playerHand();
    }

    private Match getMatch(UUID matchId) {
        Match match = matches.get(matchId);
        if (match == null) throw new IllegalArgumentException("Invalid match ID");
        return match;
    }


    private List<Card> fullDeck() {
        List<Card> deck = new ArrayList<>();
        String[] colors = { "Red", "Green", "Blue", "Yellow" };

        for (String color : colors) {
            deck.add(new NumberCard(color, 0));
            for (int i = 1; i <= 9; i++) {
                deck.add(new NumberCard(color, i));
                deck.add(new NumberCard(color, i)); // dos de cada número
            }
            for (int i = 0; i < 2; i++) {
                deck.add(new SkipCard(color));
                deck.add(new ReverseCard(color));
                deck.add(new Draw2Card(color));
            }
        }
        for (int i = 0; i < 4; i++) {
            deck.add(new WildCard());
        }

        return deck;
    }
}
