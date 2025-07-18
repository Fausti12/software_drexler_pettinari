package org.udesa.unoapplication.model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

public class Match {
    public static String InvalidNumberOfPlayers = "Number of players must be at least 2";
    public static String EmptyPlayersName = "Player with no name";
    public static String NotACardInHand = "Not a card in hand of ";
    public static String CardDoNotMatch = "Card does not match Color, Number or Kind";
    public static String DuplicatePlayerNames = "There are duplicate player names.";
    private Function<GameStatus, GameStatus> reverseShift;
    private Function<GameStatus, GameStatus> nextShift;
    private GameStatus status;
    private Card discardPileHead;
    private List<Card> drawPile;

    public static Match newReducedMatch(List<Card> deck, String... players) {
        return new Match(new ArrayList(deck), 5, List.of(players));
    }

    public static Match fullMatch(List<Card> deck, List<String> players) {
        return new Match(new ArrayList(deck), 7, players);
    }

    public Match(List<Card> deck, int cardsInHand, List<String> players) {
        checkValidPlayerList(players); //se chequea si hay mínimo 2 jugadores

        discardPileHead = deck.remove(0);
        nextShift = (status) -> status.right();
        reverseShift = (status) -> status.left();

        Playing first = new Playing(players.getFirst(), new ArrayList(deck.subList(0, cardsInHand)));
        deck.subList(0, cardsInHand).clear();

        Playing st = first;
        for (String player : players.subList(1, players.size())) {
            st = new Playing(st, player, new ArrayList(deck.subList(0, cardsInHand)));
            deck.subList(0, cardsInHand).clear();
        }
        first.linkTo(st);
        drawPile = deck;
        status = first;
    }

    public void drawCard(String player) {
        status.assertTurnOf(player);
        draw();
    }

    public void play(String playerName, Card aCard) {
        status.assertTurnOf(playerName);
        Player player = status.player();

        if (!player.hasCard(aCard)) {
            throw new RuntimeException(NotACardInHand + playerName);
        }

        if (!discardPileHead.acceptsOnTop(aCard)) {
            throw new RuntimeException(CardDoNotMatch);
        }

        player.removeCard(aCard);
        discardPileHead = aCard;

        if (player.oneCardLeft() && !aCard.unoShouted()) {
            draw();
            draw();
        }

        if (player.noCardLeft()) {
            status = new GameOver(player);
        } else {
            aCard.affectGame(this);
        }

    }

    public Card activeCard() {
        return discardPileHead;
    }

    public List<Card> playerHand() {
        return status.player().hand();
    }

    public boolean acceptsColor(String aColor) {
        return discardPileHead.yourColorIs(aColor);
    }

    public void reverseShifturn() {
        Function<GameStatus, GameStatus> temp = nextShift;
        nextShift = reverseShift;
        reverseShift = temp;
    }

    public void shiftTurn() {
        status = nextShift.apply(status);
    }

    public void draw() {
        status.player().drawCard(drawPile.remove(0));
    }

    public boolean isOver() {
        return status.isOver();
    }

    private void checkValidPlayerList(List<String> players) {
        if (players.stream().anyMatch(name -> name.trim().isEmpty())) {
            throw new IllegalArgumentException(EmptyPlayersName);
        }

        if (players.size() < 2) {
            throw new IllegalArgumentException(InvalidNumberOfPlayers);
        }

        // Hay nombres repetidos
        Set<String> uniqueNames = new HashSet<>(players);
        if (uniqueNames.size() != players.size()) {
            throw new IllegalArgumentException(DuplicatePlayerNames);
        }

    }

}
