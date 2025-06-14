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
import org.udesa.unoapplication.service.Dealer;
import org.udesa.unoapplication.service.UnoService;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
public class UnoControllerTest {

    @Autowired MockMvc mockMvc;
    @MockBean UnoService unoService;
    @MockBean private Dealer dealer;
    private UUID matchId;

    @BeforeEach
    public void setup() throws Exception {
        List<Card> mazo = List.of(new NumberCard("Red", 1), // va al pozo
                // Cartas de Miguel
                new NumberCard("Red", 7),
                new NumberCard("Red", 2),
                new NumberCard("Blue", 3),
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
                new NumberCard("Red", 7),
                // Cartas que quedan en el mazo
                new NumberCard("Blue", 1),
                new NumberCard("Blue", 2),
                new NumberCard("Blue", 3)
        );
        when(dealer.fullDeck()).thenReturn(mazo);

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
        // assertNotNull(card);
        assertEquals(new JsonCard("Red", 1, "NumberCard", false).toString(), card.toString());
    }

    @Test
    public void test03CanGetPlayerHand() throws Exception {
        List<JsonCard> hand = getPlayerHand(matchId);
        assertFalse(hand.isEmpty());
        assertEquals(7, hand.size());
        assertEquals(new JsonCard("Red", 7, "NumberCard", false).toString(), hand.getFirst().toString());
    }

    @Test
    public void test04CanDrawCard() throws Exception {
        List<JsonCard> MiguelBefore = getPlayerHand(matchId);
        draw(matchId, "Miguel"); // el draw no hace que se pase el turno
        List<JsonCard> MiguelAfter = getPlayerHand(matchId);
        assertEquals(MiguelBefore.size() + 1, MiguelAfter.size()); // Miguel tiene una carta más que antes
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
    public void test05CannotCreateMatchWithDuplicateNames() throws Throwable {
        String resp = mockMvc.perform(post("/newmatch?players=Ana&players=Ana"))
                .andDo(print())
                .andExpect(status().is(400))
                .andReturn().getResponse().getContentAsString();

        assertEquals("Error: " + Match.DuplicatePlayerNames, resp);
    }

    /* ESTE TEST ES IGUAL AL SIGUIENTE
    @Test
    public void test06PlayInvalidMatchThrows() throws Exception {
        mockMvc.perform(post("/play/" + UUID.randomUUID() + "/Miguel")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(json(new JsonCard("Blue", 1, "NumberCard", false))))
                .andDo(print())
                .andExpect(status().isBadRequest());
    }
    */

    @Test
    public void test07CanNotPlayOnInvalidMatchId() throws Exception {
    mockMvc.perform(post("/play/" + UUID.randomUUID() + "/Miguel")
           .contentType(MediaType.APPLICATION_JSON)
           .content(json(new JsonCard("Red", 2, "NumberCard", false))))
           .andDo(print())
           .andExpect(status().isBadRequest());
    }

    @Test
    public void test08CannotGetHandOfInvalidMatch() throws Exception {
        mockMvc.perform(get("/playerhand/" + UUID.randomUUID()))
               .andExpect(status().isBadRequest());
    }

    // ESTO SERÍA UN CHEQUEO DEL MODELO PARA MÍ
    /*
    @Test
    public void test08CannotPlayOutOfTurn() throws Exception {
        JsonCard card = getPlayerHand(matchId).getFirst(); // Miguel tiene el turno, Jorge no

        mockMvc.perform(post("/play/" + matchId + "/Jorge")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(json(card)))
                .andDo(print())
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test09CannotPlayInvalidCard() throws Exception {
        // Miguel tiene el turno, pero juega una carta que no matchea con el pozo (ej: Blue 3)
        JsonCard invalid = new JsonCard("Blue", 3, "NumberCard", false);

        mockMvc.perform(post("/play/" + matchId + "/Miguel")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(json(invalid)))
                .andDo(print())
                .andExpect(status().isBadRequest());
    }
    */

    @Test
    public void test09PlayWithoutCardContent() throws Exception {
        mockMvc.perform(post("/play/" + matchId + "/Miguel")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test10CannotCreateMatchWithNoPlayers() throws Exception {
        mockMvc.perform(post("/newmatch"))
                .andExpect(status().isBadRequest());
    }

    /* TESTEADO EN UnoTest --> testNumberOfPlayersInvalid()
    @Test
    public void test11CannotCreateMatchWithOnePlayer() throws Exception {
        mockMvc.perform(post("/newmatch?players=Gerardo"))
                .andExpect(status().isBadRequest());
    }
    */

    /* TESTEADO EN UnoTest --> testPlayersWithNoName()
    @Test
    public void test12CannotCreateMatchWithEmptyPlayerNames() throws Exception {
        mockMvc.perform(post("/newmatch?players=&players="))
                .andExpect(status().isBadRequest());
    }
    */

    // TEST DE JULIO =================================================================
    @Test
    void playingWrongTurnTest() throws Exception{
        // crear nuevo juego
        UUID uuid = newMatch("Emilio", "Julio");
        assertNotNull(uuid);

        // poner disponibles las cartas necesarias
        List<JsonCard> cards = getPlayerHand(uuid);

        // probar que devuelve el texto del error, sin tener la aplicación corriendo
        String resp = mockMvc.perform(post("/play/" + uuid + "/Julio")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(cards.getFirst().toString()))
                .andDo(print())
                .andExpect(status().is(400)).andReturn().getResponse().getContentAsString();

        // assertar que el mensaje es correcto
        assertEquals("Error: " + Player.NotPlayersTurn + "Julio", resp);
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
