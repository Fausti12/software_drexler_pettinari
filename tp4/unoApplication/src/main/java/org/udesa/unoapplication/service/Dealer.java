package org.udesa.unoapplication.service;

import org.springframework.stereotype.Component;
import org.udesa.unoapplication.model.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

@Component
public class Dealer {

    public List<Card> fullDeck(){
        ArrayList<Card> deck = new ArrayList<>();
        deck.addAll(cardsOn("Red"));
        deck.addAll(cardsOn("Green"));
        deck.addAll(cardsOn("Blue"));
        deck.addAll(cardsOn("Yellow"));
        Collections.shuffle(deck);
        return deck;
    }

    private List<Card> cardsOn(String color){
        return List.of(new WildCard(), new SkipCard(color),
                new Draw2Card(color), new NumberCard(color, 1),
                new NumberCard(color, 2), new NumberCard(color, 3),
                new NumberCard(color, 4), new NumberCard(color, 5));
    }

}
