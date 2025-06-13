package org.udesa.unoapplication.model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import static org.mockito.Mockito.when;
import org.udesa.unoapplication.service.Dealer;
import org.udesa.unoapplication.service.UnoService;
import org.udesa.unoapplication.service.service;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
public class UnoServiceTest {
    @Autowired private service unoService; //dsp poner UnoService class
    @MockBean private Dealer dealer;
    private UUID matchId;

    @BeforeEach
    public void setup() {
        List<Card> mazo = List.of(new NumberCard("Red", 1), // va al pozo
        // Cartas de Miguel
        new NumberCard("Red", 1),
        new NumberCard("Red", 2),
        new NumberCard("Red", 3),
        new NumberCard("Red", 4),
        new NumberCard("Red", 5),
        new NumberCard("Red", 6),
        new NumberCard("Red", 7),
        // Cartas de Jorge
        new NumberCard("Yellow", 1),
        new NumberCard("Yellow", 2),
        new NumberCard("Yellow", 3),
        new NumberCard("Yellow", 4),
        new NumberCard("Yellow", 5),
        new NumberCard("Yellow", 6),
        new NumberCard("Red", 7)
        );
        when(dealer.fullDeck()).thenReturn(mazo);


        matchId = unoService.newMatch(List.of("Miguel", "Jorge"));
    }

    @Test public void testNewMatch() {
        assertNotNull(matchId);
    }

    @Test public void TestActualPlayerHandIsAccessible() {
        List<Card> hand = unoService.playerHand(matchId);
        assertFalse(hand.isEmpty());
    }

    @Test public void canPlayCardTest() {
        List<Card> before = unoService.playerHand(matchId);
        unoService.play(matchId, "Miguel", new NumberCard("Red", 4));
        unoService.play(matchId, "Jorge", new NumberCard("Yellow", 4));
        List<Card> after = unoService.playerHand(matchId);
        assertEquals(before.size(), after.size() + 1);
    }

    @Test public void TestCanDrawCard() {
        List<Card> before = unoService.playerHand(matchId);
        unoService.drawCard(matchId, "Miguel");
        List<Card> after = unoService.playerHand(matchId);
        assertEquals(before.size() + 1, after.size());
    }

    @Test public void TestActiveCardIsNotNull() {
        Card active = unoService.activeCard(matchId);
        assertNotNull(active);
    }

}
