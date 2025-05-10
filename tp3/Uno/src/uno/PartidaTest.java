package uno;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

public class PartidaTest {
    private Carta r2, r3, r4, r5, r6, a2, a7;

    @BeforeEach public void setUp(){
        r2 = new CartaNumero(Color.ROJO, 2); //se podria pasar Strings y que Juego cree clase Carta
        r3 = new CartaNumero(Color.ROJO, 3);
        r4 = new CartaNumero(Color.ROJO, 4);
        r5 = new CartaNumero(Color.ROJO, 5);
        r6 = new CartaNumero(Color.ROJO, 6);
        a2 = new CartaNumero(Color.AZUL, 2);
        a7 = new CartaNumero(Color.AZUL, 7);

    }
    @Test void testPozoInicialConUnaCarta() {
        Juego j = new Juego(List.of(r2), "juan", "pedro");
        assertEquals(Color.ROJO, j.colorPozo());
        assertEquals(2, j.numPozo());
    }

    @Test void testPozoInicialConVariasCartas() {
        Juego j = new Juego(List.of(r2,r3,r4), "juan", "pedro");
        assertEquals(Color.ROJO, j.colorPozo());
        assertEquals(2, j.numPozo());
    }

    @Test void testCantidadDeCartasPorJugador() {
        Juego j = new Juego(List.of(r2,r3,r4,r5,r6), "juan", "pedro");

        assertEquals(2, j.cartasJugador("juan"));
        assertEquals(2, j.cartasJugador("pedro"));
    }



    @Test void testJugadorJuegaCartaPorColor() {
        Juego j = new Juego(List.of(r2, r4, a7), "juan", "pedro");
        j.jugarCarta(r4);

        assertEquals(Color.ROJO, j.colorPozo());
        assertEquals(4, j.numPozo());
    }

    @Test void testJugadorJuegaCartaPorNumero() {
        Juego j = new Juego(List.of(r2, a2), "juan", "pedro");
        j.jugarCarta(a2);

        assertEquals(Color.AZUL, j.colorPozo());
        assertEquals(2, j.numPozo());
    }

    @Test void testJugadorJuegaCartaEquivocada() {
        Juego j = new Juego(List.of(r2, r3, a7), "juan", "pedro");
        assertThrows(Throwable.class, () -> j.jugarCarta(a7));
    }

    @Test void testJugadorRobaCartaYNoTira() {
        Juego j = new Juego(List.of(r2, r3, r4, r5, r6), 1, "juan", "pedro");
        assertEquals(1, j.cartasJugador("juan"));
        j.agarrarCartaMazo();
        assertEquals(2, j.cartasJugador("juan"));
        j.pasaTurno();
        assertEquals("pedro", j.nombreJugadorDelTurno());
    }

    @Test void testJugadorRobaCartaYTira() {
        Juego j = new Juego(List.of(r2, r3, r4, r5, r6), 1, "juan", "pedro");
        assertEquals(1, j.cartasJugador("juan"));
        j.agarrarCartaMazo();
        assertEquals(2, j.cartasJugador("juan"));
        j.jugarCarta(r5);  //solo puede tirar la que agarró
        assertEquals(1, j.cartasJugador("juan"));
    }


      //ver más adelante
        @Test
        void testJugadorCantaUno() {
            Carta r2 = new CartaNumero(Color.ROJO, 2);
            Carta r4 = new CartaNumero(Color.ROJO, 4);

            Juego j = new Juego(List.of(r2, r4), "juan", "pedro");
            //j.jugarCarta(r4.uno());

            assertEquals(Color.ROJO, j.colorPozo());
            assertEquals(4, j.numPozo());
            //assertTrue(j.cantoUno("juan"));
        }

        @Test
        void testJugadorNoCantaUnoDebeRobar() {
            Carta r2 = new CartaNumero(Color.ROJO, 2);
            Carta r4 = new CartaNumero(Color.ROJO, 4);

            Juego j = new Juego(List.of(r2, r4), "juan", "pedro");
            j.jugarCarta(r4);

            assertEquals(2, j.cartasJugador("juan")); // tenía 1, no cantó, debe robar 2
        }

}


