package uno;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

public class PartidaTest {
    private Carta r2, r3, r4, r5, r6, r7, r8, a2, a7, tomaDos, saltaAzul, saltaRojo, comodin;

    @BeforeEach public void setUp(){
        r2 = new CartaNumero(Color.ROJO, 2); //se podria pasar Strings y que Juego cree clase Carta
        r3 = new CartaNumero(Color.ROJO, 3);
        r4 = new CartaNumero(Color.ROJO, 4);
        r5 = new CartaNumero(Color.ROJO, 5);
        r6 = new CartaNumero(Color.ROJO, 6);
        r7 = new CartaNumero(Color.ROJO, 7);
        r8 = new CartaNumero(Color.ROJO, 8);
        a2 = new CartaNumero(Color.AZUL, 2);
        a7 = new CartaNumero(Color.AZUL, 7);
        tomaDos = new CartaDraw2(Color.AZUL);
        saltaAzul = new CartaSkip(Color.AZUL);
        saltaRojo = new CartaSkip(Color.ROJO);
        comodin = new CartaWild();
    }

    @Test void testPozoInicialConUnaCarta() {
        Juego j = new Juego(List.of(r2), 0, "juan", "pedro");
        assertEquals(Color.ROJO, j.colorPozo());
        assertEquals(2, j.numPozo());
    }

    @Test void testPozoInicialConVariasCartas() {
        Juego j = new Juego(List.of(r2,r3,r4), 1,  "juan", "pedro");
        assertEquals(Color.ROJO, j.colorPozo());
        assertEquals(2, j.numPozo());
    }

    @Test void testCantidadDeCartasPorJugador() {
        Juego j = new Juego(List.of(r2,r3,r4,r5,r6), 2, "juan", "pedro");
        assertEquals(2, j.cartasJugador("juan"));
        assertEquals(2, j.cartasJugador("pedro"));
    }


    @Test void testJugadorJuegaCartaPorColor() {
        Juego j = new Juego(List.of(r2, r4, a7), 1, "juan", "pedro");
        j.jugarCarta(r4);

        assertEquals(Color.ROJO, j.colorPozo());
        assertEquals(4, j.numPozo());
    }

    @Test void testJugadorJuegaCartaPorNumero() {
        Juego j = new Juego(List.of(r2, a2, r3), 1, "juan", "pedro");
        j.jugarCarta(a2);

        assertEquals(Color.AZUL, j.colorPozo());
        assertEquals(2, j.numPozo());
    }

    @Test void testJugadorJuegaCartaNoPosible() {
        Juego j = new Juego(List.of(r2, a7, r3), 1, "juan", "pedro");
        assertThrows(Throwable.class, () -> j.jugarCarta(a7));
    }

    @Test void testJugadorJuegaCartaQueNoTiene() {
        Juego j = new Juego(List.of(r2, r3, r4, r5, r6), 2, "juan", "pedro");
        assertThrows(Throwable.class, () -> j.jugarCarta(a7));
    }

    @Test void testJugadorJuegaCartaQueYaTiro() {
        Juego j = new Juego(List.of(r2, r3, r4, r5, r6), 2, "juan", "pedro");
        j.jugarCarta(r3);
        assertEquals("pedro", j.nombreJugadorDelTurno());
        j.jugarCarta(r4);
        assertThrows(Throwable.class, () -> j.jugarCarta(r3));
    }

    @Test void testJugadorJuegaLasDosCartasRepetidas() {
        Juego j = new Juego(List.of(r2, r3, r4, r3, r6, r4, a7), 3, "juan", "pedro");
        j.jugarCarta(r3);
        j.jugarCarta(r4); //juega pedro
        j.jugarCarta(r3);
        assertEquals(1, j.cartasJugador("juan"));
    }

    @Test void testJugadorRobaCartaYNoTira() {
        Juego j = new Juego(List.of(r2, r3, r4, r5, r6), 1, "juan", "pedro");
        assertEquals(1, j.cartasJugador("juan"));
        j.agarrarCartaMazo();
        assertEquals(2, j.cartasJugador("juan"));
        j.avanzarTurno();
        assertEquals("pedro", j.nombreJugadorDelTurno());
    }

    @Test void testJugadorRobaCartaYTiraMismaCarta() {
        Juego j = new Juego(List.of(r2, r3, r4, r5, r6), 1, "juan", "pedro");
        assertEquals(1, j.cartasJugador("juan"));
        j.agarrarCartaMazo();
        assertEquals(2, j.cartasJugador("juan"));
        j.jugarCarta(r5);  //solo puede tirar la que agarró
        assertEquals(1, j.cartasJugador("juan"));
    }

    @Test void testJugadorRobaCartaYTiraDistintaCarta() {
        Juego j = new Juego(List.of(r2,r3, r4, r5, r6), 1, "juan", "pedro");
        assertEquals(1, j.cartasJugador("juan"));
        j.agarrarCartaMazo();
        assertEquals(2, j.cartasJugador("juan"));
        assertThrows(Throwable.class, () -> j.jugarCarta(r3));
    }

    //tests sobre carta Draw2
    @Test void testCartaInicialEnPozoEsDraw2() {
        Juego j = new Juego(List.of(tomaDos, r2, r3, r4, r5, r6), 1, "juan", "pedro");
        assertEquals("AZUL draw2", j.tipoCartaPozo());
    }

    @Test void testJugadorAgarraCartasAlHaberDraw2EnPozoInicial() {
        Juego j = new Juego(List.of(tomaDos, r2, r3, r4, r5, r6), 1, "juan", "pedro");
        assertEquals(3, j.cartasJugador("juan"));
        assertEquals(1, j.cartasJugador("pedro"));
    }


    @Test void testJugadorAgarraCartasLuegoQueOtroTireDraw2Valido() {
        Juego j = new Juego(List.of(a7, tomaDos, r4, r5, r6, r7, r8), 1, "juan", "pedro");
        //pozo: a7, juan: tiene draw2 azul, pedro: tiene r4
        j.jugarCarta(tomaDos);
        assertEquals(3, j.cartasJugador("pedro"));
    }

    //hacer test en el que tiren 2 cartas draw2 seguidas

    @Test void testJugadorTiraDraw2NoValido() {
        Juego j = new Juego(List.of(r2, tomaDos, r4, r5, r6, r7, r8), 1, "juan", "pedro");
        assertThrows(Throwable.class, () -> j.jugarCarta(tomaDos));
    }


    //tests sobre carta Skip

    @Test void testCartaInicialEnPozoEsSkip() {
        Juego j = new Juego(List.of(saltaAzul, r2, r3, r4, r5, r6), 1, "juan", "pedro");
        assertEquals("SKIP AZUL", j.tipoCartaPozo());
    }

    @Test void testJugadorEsSaltadoConSkipInicial() {
        Juego j = new Juego(List.of(saltaAzul, r2, r3, r4, r5, r6), 1, "juan", "pedro");
        assertEquals("pedro", j.nombreJugadorDelTurno()); // turno salta a pedro
    }

    @Test void testJugadorTiraSkipValidoYSaltaAlSiguiente() {
        Juego j = new Juego(List.of(r2, saltaRojo, r4, r5, r6, r7, r8), 1, "juan", "pedro", "luis");
        j.jugarCarta(saltaRojo);
        assertEquals("luis", j.nombreJugadorDelTurno()); // salta a luis
    }

    @Test void testDosJugadoresTiranSkipSeguidosValidos() {
        Juego j = new Juego(List.of(r2, saltaRojo, r4, saltaAzul, r6, r7, r8), 1, "juan", "pedro", "luis");
        j.jugarCarta(saltaRojo);
        j.jugarCarta(saltaAzul);
        assertEquals("pedro", j.nombreJugadorDelTurno());
    }

    @Test void testJugadorTiraSkipNoValido() {
        Juego j = new Juego(List.of(r2, saltaAzul, r4, r5, r6, r7, r8), 1, "juan", "pedro");
        assertThrows(Throwable.class, () -> j.jugarCarta(saltaAzul));
    }


    // tests de carta comodin

    @Test void testCartaInicialEnPozoEsWild() {
        Juego j = new Juego(List.of(comodin, r2, r3, r4, r5, r6), 1, "juan", "pedro");
        assertEquals("WILD(NINGUNO)", j.tipoCartaPozo());
    }

    @Test void testJugadorEligeColorYTiraConWildInicial() {
        Juego j = new Juego(List.of(comodin, r2, r3, r4, r5, r6), 2, "juan", "pedro");
        j.asignarColorAComodin(Color.ROJO);
        assertEquals("WILD(ROJO)", j.tipoCartaPozo());
        j.jugarCarta(r2);
        assertEquals(1, j.cartasJugador("juan"));
    }

    @Test void testJugadorEligeColorYTiraCartaErroneaConWildInicial() {
        Juego j = new Juego(List.of(comodin, r2, r3, a2, r5, r6), 2, "juan", "pedro");
        j.asignarColorAComodin(Color.ROJO);
        assertEquals("WILD(ROJO)", j.tipoCartaPozo());
        assertThrows(Throwable.class, () -> j.jugarCarta(a2));
    }








      //ver más adelante
        @Test
        void testJugadorCantaUno() {
            Carta r2 = new CartaNumero(Color.ROJO, 2);
            Carta r4 = new CartaNumero(Color.ROJO, 4);

            Juego j = new Juego(List.of(r2, r4), 1, "juan", "pedro");
            //j.jugarCarta(r4.uno());

            assertEquals(Color.ROJO, j.colorPozo());
            assertEquals(4, j.numPozo());
            //assertTrue(j.cantoUno("juan"));
        }

        @Test
        void testJugadorNoCantaUnoDebeRobar() {
            Carta r2 = new CartaNumero(Color.ROJO, 2);
            Carta r4 = new CartaNumero(Color.ROJO, 4);

            Juego j = new Juego(List.of(r2, r4) , 1, "juan", "pedro");
            j.jugarCarta(r4);

            assertEquals(2, j.cartasJugador("juan")); // tenía 1, no cantó, debe robar 2
        }

}


