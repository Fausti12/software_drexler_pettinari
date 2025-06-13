package org.udesa.unoapplication.model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.udesa.unoapplication.service.Dealer;
import org.udesa.unoapplication.service.UnoService;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

@SpringBootTest
public class UnoServiceTest {
    @Autowired private UnoService unoService; //dsp poner UnoService class
    @MockBean Dealer dealer;
    private UUID matchId;

    @BeforeEach
    public void setup() {
        List<Card> fixedDeck = customDeckForTesting();
        when(dealer.fullDeck()).thenReturn(fixedDeck);
        matchId = unoService.newMatch(List.of("Miguel", "Jorge"));
    }


    @Test public void testNewMatch() {
        assertNotNull(matchId);
    }

    @Test public void testStartActiveCard() {
        assertNotNull(unoService.activeCard(matchId));
    }

    @Test public void testActualPlayerHandIsAccessible() {
        List<Card> hand = unoService.playerHand(matchId);
        assertTrue(hand.size() == 7);
    }

    @Test public void testActiveCardActualizate(){
        List<Card> hand = unoService.playerHand(matchId);
        unoService.play(matchId, "Miguel", hand.get(0));
        assertTrue(unoService.activeCard(matchId).equals(hand.get(0)));
    }

    @Test public void testCanPlayCard() {
        List<Card> firstPlayerActualHand = unoService.playerHand(matchId);
        unoService.play(matchId, "Miguel", firstPlayerActualHand.get(0));
        List<Card> secondPlayerActualHand = unoService.playerHand(matchId);
        unoService.play(matchId, "Jorge", secondPlayerActualHand.get(0));

        List<Card> firstPlayerAfterHand = unoService.playerHand(matchId);  //mano de Miguel
        assertEquals(firstPlayerAfterHand.size() + 1 , firstPlayerActualHand.size());
    }

    @Test public void testCanDrawCard() {
        List<Card> before = unoService.playerHand(matchId);
        unoService.drawCard(matchId, "Miguel");
        List<Card> after = unoService.playerHand(matchId);
        assertEquals(before.size() + 1, after.size());
    }

    @Test public void testPlayWithInvalidMatchId() {
        UUID invalidId = UUID.randomUUID();
        Card card = unoService.playerHand(matchId).get(0);
        assertThrows(RuntimeException.class, () -> unoService.play(invalidId, "Miguel", card));
        assertThrowsLike(() -> unoService.play(invalidId, "Miguel", card), unoService.invalidId);
    }

    private void assertThrowsLike(Executable executable, String message ) {
        assertEquals( message, assertThrows( Exception.class, executable ).getMessage() );
    }



    private List<Card> customDeckForTesting() {
        return List.of(
                new NumberCard("Red", 3), //pozo inicial
                new NumberCard("Red", 5),
                new NumberCard("Green", 5),
                new Draw2Card("Red"),
                new SkipCard("Blue"),
                new ReverseCard("Green"),
                new WildCard(),
                new NumberCard("Yellow", 7),
                new NumberCard("Blue", 5),
                new NumberCard("Red", 5),
                new NumberCard("Blue", 4),
                new NumberCard("Blue", 6),
                new NumberCard("Green", 1),
                new NumberCard("Green", 2),
                new Draw2Card("Green"),
                new NumberCard("Blue", 9)  //carta para agarrar
        );
    }


}
