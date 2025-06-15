package org.udesa.unoapplication.model;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;
import org.udesa.unoapplication.controller.UnoController;
import org.udesa.unoapplication.service.Dealer;
import org.udesa.unoapplication.model.UnoServiceTest;

import java.util.List;
import java.util.UUID;

import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
public class UnoControllerTest {
    @Autowired MockMvc mockMvc;
    @MockBean Dealer dealer;


    @BeforeEach
    public void setup() throws Exception {
        when(dealer.fullDeck()).thenReturn(UnoServiceTest.customDeckForTesting());
    }


    @Test public void testCanCreateMatch() throws Exception {
        String id = newMatch("Ana", "Luis");
        assertNotNull(id);
    }

    @Test public void testCanGetActiveCard() throws Exception {
        String id = newMatch("Miguel", "Jorge");
        JsonCard card = getActiveCard(id);
        assertNotNull(card);
        assertEquals(new JsonCard("Red", 3, "NumberCard", false).toString(), card.toString());
    }

    @Test public void testCanGetPlayerHand() throws Exception {
        String id = newMatch("Miguel", "Jorge");
        List<JsonCard> hand = getPlayerHand(id);
        assertFalse(hand.isEmpty());
        assertEquals(7, hand.size());
        assertEquals(new JsonCard("Red", 5, "NumberCard", false).toString(), hand.getFirst().toString());
    }

    @Test public void testCanDrawCard() throws Exception {
        String id = newMatch("Miguel", "Jorge");
        List<JsonCard> before = getPlayerHand(id);
        draw(id, "Miguel");
        List<JsonCard> after = getPlayerHand(id);
        assertEquals(before.size() + 1, after.size());
    }

    @Test public void testCanPlayValidCard() throws Exception {
        String id = newMatch("Miguel", "Jorge");
        JsonCard cardFirstPlayer = getPlayerHand(id).getFirst();
        play(id, "Miguel", cardFirstPlayer);
        JsonCard cardSecondPlayer = getPlayerHand(id).getFirst();
        play(id, "Jorge", cardSecondPlayer);
        assertEquals(6,  getPlayerHand(id).size());
    }

    @Test public void testCanNotPlayOnInvalidMatchId() throws Exception {
        mockMvc.perform(post("/play/" + UUID.randomUUID() + "/Miguel")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(json(new JsonCard("Red", 2, "NumberCard", false))))
                .andDo(print())
                .andExpect(status().isBadRequest());
    }

    @Test public void testPlayerWrongTurn() throws Exception {
        String id = newMatch("Miguel", "Jorge");
        assertNotNull(id);
        List<JsonCard> cards = getPlayerHand(id);

        String resp = mockMvc.perform(post("/play/" + id + "/Jorge")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(cards.getFirst().toString()))
                .andDo(print())
                .andExpect(status().is(400)).andReturn().getResponse().getContentAsString();

        assertEquals("Illegal Argument: " + Player.NotPlayersTurn + "Jorge", resp);
    }



    @Test public void testCannotCreateMatchWithDuplicateNames() throws Throwable {
        String resp = mockMvc.perform(post("/newmatch?players=Ana&players=Ana"))
                .andDo(print())
                .andExpect(status().is(400))
                .andReturn().getResponse().getContentAsString();

        assertEquals("Illegal Argument: " + Match.DuplicatePlayerNames, resp);
    }

    @Test public void testPlayWithoutCardContent() throws Exception {
        String id = newMatch("Miguel", "Jorge");
        mockMvc.perform(post("/play/" + id + "/Miguel")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest());
    }

    @Test  public void testCannotCreateMatchWithNoPlayers() throws Exception {
        mockMvc.perform(post("/newmatch"))
                .andExpect(status().isBadRequest());
    }


    @Test public void testPlayWithMalformedJson() throws Exception {
        String malformedJson = "{\"color\":\"Red\",\"number\":3,\"type\":\"NumberCard\",\"shout\":false"; // falta cierre }

        String resp = mockMvc.perform(post("/play/" + UUID.randomUUID() + "/Miguel")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(malformedJson))
                .andDo(print())
                .andExpect(status().isBadRequest())
                .andReturn()
                .getResponse()
                .getContentAsString();

        assertTrue(resp.startsWith("Malformed or incomplete JSON:"), "Mensaje: " + resp);
    }



    private String newMatch(String... players) throws Exception {
        StringBuilder url = new StringBuilder("/newmatch");
        for (String player : players)
            url.append(url.indexOf("?") == -1 ? "?players=" : "&players=").append(player);

        String content = mockMvc.perform(post(url.toString()))
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();

        return new ObjectMapper().readTree(content).asText();
    }

    private void play(String matchId, String player, JsonCard card) throws Exception {
        mockMvc.perform(post("/play/" + matchId + "/" + player)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(json(card)))
                .andDo(print())
                .andExpect(status().is(200));
    }

    private void draw(String matchId, String player) throws Exception {
        mockMvc.perform(post("/draw/" + matchId + "/" + player))
                .andDo(print())
                .andExpect(status().is(200));
    }

    private JsonCard getActiveCard(String matchId) throws Exception {
        String content = mockMvc.perform(get("/activecard/" + matchId))
                .andDo(print())
                .andExpect(status().is(200))
                .andReturn()
                .getResponse()
                .getContentAsString();
        return new ObjectMapper().readValue(content, JsonCard.class);
    }

    private List<JsonCard> getPlayerHand(String matchId) throws Exception {
        String content = mockMvc.perform(get("/playerhand/" + matchId))
                .andDo(print())
                .andExpect(status().is(200))
                .andReturn()
                .getResponse()
                .getContentAsString();
        return new ObjectMapper().readValue(content, new TypeReference<List<JsonCard>>() {});
    }

    private String json(Object o) throws JsonProcessingException {
        return new ObjectMapper().writeValueAsString(o);
    }


}
