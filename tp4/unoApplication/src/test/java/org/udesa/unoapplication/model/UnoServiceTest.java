package org.udesa.unoapplication.model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.udesa.unoapplication.service.UnoService;
import org.udesa.unoapplication.service.service;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
public class UnoServiceTest {
    @Autowired private service unoService; //dsp poner UnoService class

    private UUID matchId;

    @BeforeEach
    public void setup() {
        matchId = unoService.newMatch(List.of("Miguel", "Jorge"));
    }

    @Test public void newMatchTest() {
        assertNotNull(matchId);
    }

    @Test public void actualPlayerHandIsAccessibleTest() {
        List<Card> hand = unoService.playerHand(matchId);
        assertFalse(hand.isEmpty());
    }
    /*
    @Test public void canPlayCardTest() {
        List<Card> before = unoService.playerHand(matchId);
        unoService.play(matchId, "Miguel");
        List<Card> after = unoService.playerHand(matchId);
        assertEquals(before.size() + 1, after.size());
    }
*/
    @Test public void canDrawCardTest() {
        List<Card> before = unoService.playerHand(matchId);
        unoService.drawCard(matchId, "Miguel");
        List<Card> after = unoService.playerHand(matchId);
        assertEquals(before.size() + 1, after.size());
    }

    @Test public void activeCardIsNotNullTest() {
        Card active = unoService.activeCard(matchId);
        assertNotNull(active);
    }

}
