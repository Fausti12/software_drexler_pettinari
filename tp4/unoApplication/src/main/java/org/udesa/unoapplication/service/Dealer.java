package org.udesa.unoapplication.service;

import org.springframework.stereotype.Component;
import org.udesa.unoapplication.model.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

@Component
public class Dealer {
    private static final List<String> COLORS = List.of("Red", "Blue", "Green", "Yellow");

    public List<Card> fullDeck() {
        List<Card> deck = new ArrayList<>();

        COLORS.forEach(color -> {
            deck.addAll(cardsForAColor(color));
        });

        deck.addAll(wildCards());
        Collections.shuffle(deck);

        return deck;
    }

    private List<Card> cardsForAColor(String color) {
        List<Card> cards = new ArrayList<>();
        cards.add(new NumberCard(color, 0));

        for (int i = 1; i <= 9; i++) {
            cards.add(new NumberCard(color, i));
            cards.add(new NumberCard(color, i));
        }

        for (int i = 0; i < 2; i++) {
            cards.add(new ReverseCard(color));
            cards.add(new Draw2Card(color));
            cards.add(new SkipCard(color));
        }

        return cards;
    }

    private List<Card> wildCards() {
        List<Card> cards = new ArrayList<>();

        for (int i = 0; i < 4; i++) {
            cards.add(new WildCard());
        }

        return cards;
    }



}
