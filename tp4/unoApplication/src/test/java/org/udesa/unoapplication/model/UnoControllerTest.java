package org.udesa.unoapplication.model;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;
import org.udesa.unoapplication.service.UnoService;

import java.util.List;
import java.util.UUID;

import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
public class UnoControllerTest {
    @Autowired MockMvc mockMvc;
    @Autowired UnoService unoService;

    private UUID matchId;

    @BeforeEach
    public void setup() throws Exception {
        matchId = newMatch("Miguel", "Jorge");
    }

    @Test
    public void test01CanCreateMatch() throws Exception {
        UUID id = newMatch("Ana", "Luis");
        assertNotNull(id);
    }

    @Test
    public void test02CanGetActiveCard() throws Exception {
        JsonCard card = getActiveCard(matchId);
        assertNotNull(card);
    }

    @Test
    public void test03CanGetPlayerHand() throws Exception {
        List<JsonCard> hand = getPlayerHand(matchId);
        assertFalse(hand.isEmpty());
    }

    @Test
    public void test04CanDrawCard() throws Exception {
        List<JsonCard> before = getPlayerHand(matchId);
        draw(matchId, "Miguel");
        List<JsonCard> after = getPlayerHand(matchId);
        assertEquals(before.size() + 1, after.size());
    }
/*
    @Test
    public void test05CanPlayCard() throws Exception {
        JsonCard card = getPlayerHand(matchId).get(0);
        play(matchId, "Miguel", card);
        // No exception = éxito
    }
*/


    @Test
    public void test06PlayInvalidMatchThrows() throws Exception {
        mockMvc.perform(post("/play/" + UUID.randomUUID() + "/Miguel")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(json(new JsonCard("Blue", 1, "NumberCard", false))))
                .andDo(print())
                .andExpect(status().is(400));
    }

    @Test public void test07CanNotPlayOnInvalidMatchId() throws Exception {
    mockMvc.perform(post("/play/" + UUID.randomUUID() + "/Miguel")
           .contentType(MediaType.APPLICATION_JSON)
           .content(json(new JsonCard("Red", 2, "NumberCard", false))))
           .andDo(print())
           .andExpect(status().isBadRequest());
    }
    


    // === Métodos auxiliares ===

    private UUID newMatch(String... players) throws Exception {
        StringBuilder url = new StringBuilder("/newmatch");
        for (String player : players)
            url.append(url.indexOf("?") == -1 ? "?players=" : "&players=").append(player);

        String raw = mockMvc.perform(post(url.toString()))
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();

        return UUID.fromString(raw.replace("\"", ""));
    }

    private void play(UUID matchId, String player, JsonCard card) throws Exception {
        mockMvc.perform(post("/play/" + matchId + "/" + player)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(json(card)))
                .andDo(print())
                .andExpect(status().is(200));
    }

    private void draw(UUID matchId, String player) throws Exception {
        mockMvc.perform(post("/draw/" + matchId + "/" + player))
                .andDo(print())
                .andExpect(status().is(200));
    }

    private JsonCard getActiveCard(UUID matchId) throws Exception {
        String content = mockMvc.perform(get("/activecard/" + matchId))
                .andDo(print())
                .andExpect(status().is(200))
                .andReturn()
                .getResponse()
                .getContentAsString();
        return new ObjectMapper().readValue(content, JsonCard.class);
    }

    private List<JsonCard> getPlayerHand(UUID matchId) throws Exception {
        String content = mockMvc.perform(get("/playerhand/" + matchId))
                .andDo(print())
                .andExpect(status().is(200))
                .andReturn()
                .getResponse()
                .getContentAsString();
        ObjectMapper mapper = new ObjectMapper();
        return mapper.readValue(content, new TypeReference<List<JsonCard>>() {});
    }

    private String json(Object o) throws JsonProcessingException {
        return new ObjectMapper().writeValueAsString(o);
    }


}
