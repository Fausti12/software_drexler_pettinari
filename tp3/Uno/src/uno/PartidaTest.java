package uno;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

public class PartidaTest {
    private Carta r2, r3, r4, r5, r6, r7, r8, a2, a6, a7, tomaDosAzul, reversaAzul, reversaRojo,
            saltaAzul, saltaRojo, comodin, comodin4;

    private List<Carta> mazoBasicoRojo, mazoBasico2, mazoBasico3, mazoRepetidas, mazoRobarYJugar,
    mazoRobarHastaPoderTirar, mazoRobarSinPoder, mazoRobarYJugarNoValida,  mazoConDraw2Inicial,
    mazoConDraw2ParaJugador1, mazoConDraw2NoJugableConPozoInicial, mazoConSegundaCartaReverse, mazoConReverseEnMedio,
    mazoConReverseNoJugable, mazoConSkipInicial, mazoConSegundaCartaSkip, mazoConDosSkips, mazoConSkipNoJugable,
    mazoConComodinInicial, mazoConComodinInicialYJugadorJuega, mazoConComodinInicialYJugadorTiraCartaNoValida,
    mazoConSegundaCartaComodin, mazoConSegundaCartaComodin4, mazoJugadorGana, mazoJugadorGanaYSeSaltea,
    mazoVariosJugadoresGanan;


    @BeforeEach public void setUp(){
        r2 = new CartaNumero(Color.ROJO, 2);
        r3 = new CartaNumero(Color.ROJO, 3);
        r4 = new CartaNumero(Color.ROJO, 4);
        r5 = new CartaNumero(Color.ROJO, 5);
        r6 = new CartaNumero(Color.ROJO, 6);
        r7 = new CartaNumero(Color.ROJO, 7);
        r8 = new CartaNumero(Color.ROJO, 8);
        a2 = new CartaNumero(Color.AZUL, 2);
        a6 = new CartaNumero(Color.AZUL, 6);
        a7 = new CartaNumero(Color.AZUL, 7);
        tomaDosAzul = new CartaDraw2(Color.AZUL);
        reversaAzul = new CartaReverse(Color.AZUL);
        reversaRojo = new CartaReverse(Color.ROJO);
        saltaAzul = new CartaSkip(Color.AZUL);
        saltaRojo = new CartaSkip(Color.ROJO);
        comodin = new CartaWild();
        comodin4 = new CartaWildDraw4();

        mazoBasicoRojo = List.of(r2, r3, r4, r5, r6, r7, r8, r7, r4);
        mazoBasico2 = List.of(r2, a7, r3);
        mazoBasico3 = List.of(r2, a2, r3);
        mazoRepetidas = List.of(r2, r3, r4, r3, r6, r4, a7, r7, r8);
        mazoRobarYJugar = List.of(r2, a7, r4, a6, r6, r7, r8);
        mazoRobarHastaPoderTirar = List.of(a7, r3, r4, r5, r6, a2);
        mazoRobarSinPoder = List.of(r2, r3, r4, r5, r6);
        mazoRobarYJugarNoValida = List.of(r3, a2, r4, a7, r6, r7);

        mazoConDraw2Inicial = List.of(tomaDosAzul, r2, r3, r4, r5, r6);
        mazoConDraw2ParaJugador1 = List.of(a7, tomaDosAzul, r4, r5, r6, r7, r8);
        mazoConDraw2NoJugableConPozoInicial = List.of(r2, tomaDosAzul, r4, r5, r6, r7, r8);

        mazoConSegundaCartaReverse = List.of(r2, reversaRojo, r3, r4, r5, r6, r7, r8, a2, a7);
        mazoConReverseEnMedio = List.of(r2, r3, reversaRojo, r4, r5, r6, r7, r8, r8, a7);
        mazoConReverseNoJugable = List.of(r2, reversaAzul, r3, r4, r5, r6);

        mazoConSkipInicial = List.of(saltaAzul, r2, r3, r4, r5, r6);
        mazoConSegundaCartaSkip = List.of(r2, saltaRojo, r4, r5, r6, r7, r8, a2, a7, r5);
        mazoConDosSkips = List.of(r2, saltaRojo, r4, saltaAzul, r6, r7, r8, a2, a7, r5);
        mazoConSkipNoJugable = List.of(r2, saltaAzul, r4, r5, r6, r7, r8);

        mazoConComodinInicial = List.of(comodin, r2, r3, r4, r5, r6);
        mazoConComodinInicialYJugadorJuega = List.of(comodin, r2, r3, r4, r5, r6, r7, r8);
        mazoConComodinInicialYJugadorTiraCartaNoValida = List.of(comodin, r2, r3, a2, r5, r6);
        mazoConSegundaCartaComodin = List.of(r2, comodin, r3, a2, r5, r6, r7, r8);
        mazoConSegundaCartaComodin4 = List.of(r2, comodin4, r3, a2, r5, r6, r7, r8, a7, r7, r8);

        mazoJugadorGana = List.of(r2, r3, r4, r5, r6);
        mazoJugadorGanaYSeSaltea = List.of(r2, r3, r4, r5, r6, r7, r8);
        mazoVariosJugadoresGanan = List.of(r2, r3, r4, r5, r6, r7, a2, r8, a6, r6);

    }

    @Test void testPozoInicialConUnaCarta() {
        Juego j = new Juego(List.of(r2), 0, "juan", "pedro");
        assertEquals(Color.ROJO, j.colorPozo());
        assertEquals(2, j.numPozo());
    }

    @Test void testPozoInicialConVariasCartas() {
        Juego j = new Juego(mazoBasicoRojo, 1,  "juan", "pedro");
        assertEquals(Color.ROJO, j.colorPozo());
        assertEquals(2, j.numPozo());
    }

    @Test void testCantidadDeCartasPorJugador() {
        Juego j = new Juego(mazoBasicoRojo, 2, "juan", "pedro");
        assertEquals(2, j.cartasJugador("juan"));
        assertEquals(2, j.cartasJugador("pedro"));
    }


    @Test void testJugadorJuegaCartaPorColor() {
        Juego j = new Juego(mazoBasicoRojo, 1, "juan", "pedro");
        j.jugarCarta(new CartaNumero(Color.ROJO, 3));

        assertEquals(Color.ROJO, j.colorPozo());
        assertEquals(3, j.numPozo());
    }

    @Test void testJugadorJuegaCartaPorNumero() {
        Juego j = new Juego(mazoBasico3, 1, "juan", "pedro");
        j.jugarCarta(new CartaNumero(Color.AZUL, 2));

        assertEquals(Color.AZUL, j.colorPozo());
        assertEquals(2, j.numPozo());
    }

    @Test void testJugadorJuegaCartaNoPosible() {
        Juego j = new Juego(mazoBasico2, 1, "juan", "pedro");
        IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
                () -> j.jugarCarta(new CartaNumero(Color.AZUL, 7)));
        assertEquals("Jugada inválida", e.getMessage());
    }

    @Test void testJugadorJuegaCartaQueNoTiene() {
        Juego j = new Juego(mazoBasicoRojo, 2, "juan", "pedro");
        IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
                () -> j.jugarCarta(new CartaNumero(Color.AZUL, 2)));
        assertEquals("La carta no está en la mano", e.getMessage());
    }

    @Test void testJugadorJuegaCartaQueYaTiro() {
        Juego j = new Juego(mazoBasicoRojo, 3, "juan", "pedro");
        j.jugarCarta(new CartaNumero(Color.ROJO, 3));
        assertEquals("pedro", j.nombreJugadorDelTurno());
        j.jugarCarta(new CartaNumero(Color.ROJO, 4));
        IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
                () -> j.jugarCarta(new CartaNumero(Color.ROJO, 3)));
        assertEquals("La carta no está en la mano", e.getMessage());
    }

    @Test void testJugadorJuegaLasDosCartasRepetidas() {
        Juego j = new Juego(mazoRepetidas, 4, "juan", "pedro");
        j.jugarCarta(new CartaNumero(Color.ROJO, 3));
        j.jugarCarta(new CartaNumero(Color.ROJO, 4)); //juega pedro
        j.jugarCarta(new CartaNumero(Color.ROJO, 3));
        assertEquals(2, j.cartasJugador("juan"));
    }


    @Test void testJugadorRobaCartaYTira() {
        Juego j = new Juego(mazoRobarYJugar, 2, "juan", "pedro");
        j.agarrarCartaMazo();
        j.jugarCarta(new CartaNumero(Color.ROJO, 7));  //solo puede tirar la que agarró
        assertEquals(2, j.cartasJugador("juan"));
    }

    @Test void testJugadorRobaCartasHastaPoderTirar() {
        Juego j = new Juego(mazoRobarHastaPoderTirar, 1, "juan", "pedro");
        j.agarrarCartaMazo();
        j.agarrarCartaMazo();
        j.agarrarCartaMazo();
        j.jugarCarta(new CartaNumero(Color.AZUL, 2));
        assertEquals(3, j.cartasJugador("juan"));
    }

    @Test void testJugadorRobaCartaCuandoPodiaTirar() {
        Juego j = new Juego(mazoRobarSinPoder, 1, "juan", "pedro");
        //IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
         //       () -> j.agarrarCartaMazo());
        //assertEquals("No puede agarrar carta si tiene una jugable", e.getMessage());
        assertThrows(IllegalStateException.class, j::agarrarCartaMazo, Juego.noMoreCards);
    }

    @Test void testJugadorRobaCartaYTiraCartaNoValida() {
        Juego j = new Juego(mazoRobarYJugarNoValida, 2, "juan", "pedro");
        j.agarrarCartaMazo();
        assertThrows(IllegalArgumentException.class, () -> j.jugarCarta(new CartaNumero(Color.AZUL, 7)), Juego.invalidPlay);
    }



    //tests sobre carta Draw2
    @Test void testCartaInicialEnPozoEsDraw2() {
        Juego j = new Juego(mazoConDraw2Inicial, 1, "juan", "pedro");
        assertEquals("AZUL draw2", j.tipoCartaPozo());
    }

    @Test void testJugadorAgarraCartasAlHaberDraw2EnPozoInicial() {
        Juego j = new Juego(mazoConDraw2Inicial, 1, "juan", "pedro");
        assertEquals(3, j.cartasJugador("juan"));
        assertEquals(1, j.cartasJugador("pedro"));
    }


    @Test void testJugadorAgarraCartasLuegoQueOtroTireDraw2Valido() {
        Juego j = new Juego(mazoConDraw2ParaJugador1, 1, "juan", "pedro");
        //pozo: a7, juan: tiene draw2 azul, pedro: tiene r4
        j.jugarCarta(new CartaDraw2(Color.AZUL));
        assertEquals(3, j.cartasJugador("pedro"));
    }

    @Test void testJugadorTiraDraw2NoValido() {
        Juego j = new Juego(mazoConDraw2NoJugableConPozoInicial, 1, "juan", "pedro");
        assertThrows(IllegalArgumentException.class, () -> j.jugarCarta(tomaDosAzul), Juego.cardNotInHand);
    }


    //tests sobre carta Reverse
/*
    @Test void testCartaInicialEnPozoEsReverseYCambiaSentidoDeLaRonda() {
        Juego j = new Juego(mazoConReverse, 1, "juan", "pedro", "luis");
        assertEquals("ROJO Reverse", j.tipoCartaPozo());
        j.jugarCarta(r2); //juega juan
        assertEquals("luis", j.nombreJugadorDelTurno());
    }
*/
    @Test void testJugadorTiraReverseYDireccionCambia() {
        Juego j = new Juego(mazoConReverseEnMedio, 3,
                "juan", "pedro", "luis");
        j.jugarCarta(new CartaNumero(Color.ROJO, 3)); //juega juan
        j.jugarCarta(new CartaReverse(Color.ROJO)); // juega pedro
        assertEquals("juan", j.nombreJugadorDelTurno());
    }

    @Test void testPrimerJugadorTiraReverse() {
        Juego j = new Juego(mazoConSegundaCartaReverse, 3,
                "juan", "pedro", "luis");
        j.jugarCarta(new CartaReverse(Color.ROJO)); //juega juan
        assertEquals("luis", j.nombreJugadorDelTurno());
    }

    @Test void testJugadorTiraReverseInvalidoPorColor() {
        Juego j = new Juego(mazoConReverseNoJugable, 2, "juan", "pedro");
        assertThrows(IllegalArgumentException.class, () -> j.jugarCarta(new CartaReverse(Color.AZUL)), Juego.invalidPlay);
    }


    //tests sobre carta Skip

    @Test void testCartaInicialEnPozoEsSkip() {
        Juego j = new Juego(mazoConSkipInicial, 1, "juan", "pedro");
        assertEquals("SKIP AZUL", j.tipoCartaPozo());
    }

    @Test void testJugadorEsSaltadoConSkipInicial() {
        Juego j = new Juego(mazoConSkipInicial, 1, "juan", "pedro");
        assertEquals("pedro", j.nombreJugadorDelTurno()); // turno salta a pedro
    }

    @Test void testJugadorTiraSkipValidoYSaltaAlSiguiente() {
        Juego j = new Juego(mazoConSegundaCartaSkip, 3, "juan", "pedro", "luis");
        j.jugarCarta(new CartaSkip(Color.ROJO));
        assertEquals("luis", j.nombreJugadorDelTurno()); // salta a luis
    }

    @Test void testDosJugadoresTiranSkipSeguidosValidos() {
        Juego j = new Juego(mazoConDosSkips, 3, "juan", "pedro", "luis");
        j.jugarCarta(new CartaSkip(Color.ROJO));
        j.jugarCarta(new CartaSkip(Color.AZUL));
        assertEquals("pedro", j.nombreJugadorDelTurno());
    }

    @Test void testJugadorTiraSkipNoValido() {
        Juego j = new Juego(mazoConSkipNoJugable, 1, "juan", "pedro");
        assertThrows(IllegalArgumentException.class, () -> j.jugarCarta(new CartaSkip(Color.AZUL)), Juego.invalidPlay);
    }


    // tests de carta comodin

    @Test void testCartaInicialEnPozoEsWild() {
        Juego j = new Juego(mazoConComodinInicial, 1, "juan", "pedro");
        assertEquals("WILD", j.tipoCartaPozo());
        assertEquals(Color.NINGUNO, j.colorPozo());
    }

    @Test void testJugadorEligeColorYTiraConWildInicial() {
        Juego j = new Juego(mazoConComodinInicialYJugadorJuega, 3, "juan", "pedro");
        j.asignarColorAComodin(Color.ROJO);
        assertEquals("WILD", j.tipoCartaPozo());
        assertEquals(Color.ROJO, j.colorPozo());
        j.jugarCarta(new CartaNumero(Color.ROJO, 2));
        assertEquals(2, j.cartasJugador("juan"));
    }

    @Test void testJugadorEligeColorYTiraCartaErroneaConWildInicial() {
        Juego j = new Juego(mazoConComodinInicialYJugadorTiraCartaNoValida, 2,
                "juan", "pedro");
        j.asignarColorAComodin(Color.ROJO);
        assertEquals("WILD", j.tipoCartaPozo());
        assertEquals(Color.ROJO, j.colorPozo());
        assertThrows(IllegalArgumentException.class, () -> j.jugarCarta(new CartaNumero(Color.AZUL, 2)), Juego.invalidPlay);
    }


    @Test void testJugadorTiraWildYEligeColor() {
        Juego j = new Juego(mazoConSegundaCartaComodin, 3, "juan", "pedro");
        j.jugarCarta(new CartaWild().asignarColor(Color.ROJO));
        assertEquals("WILD", j.tipoCartaPozo());
        assertEquals(Color.ROJO, j.colorPozo());
        assertEquals(2, j.cartasJugador("juan"));
    }

    //tests de carta Comodin toma cuatro  (en instrucciones dice que puede haber un wild4 en Pozo inicial)
    @Test void testJugadorTiraWild4YEligeColor() {
        Juego j = new Juego(mazoConSegundaCartaComodin4, 3,
                "juan", "pedro");
        j.jugarCarta(new CartaWildDraw4().asignarColor(Color.ROJO));
        assertEquals("WILD4", j.tipoCartaPozo());
        assertEquals(Color.ROJO, j.colorPozo());
        assertEquals(2, j.cartasJugador("juan"));
        assertEquals(7, j.cartasJugador("pedro"));  //3 iniciales + 4 por Carta wild4
    }

    //tests de cantar uno
    @Test void testJugadorCantaUnoCuandoTieneUnaCarta() {
        Juego j = new Juego(mazoBasicoRojo, 2, "juan", "pedro");
        j.jugarCarta(new CartaNumero(Color.ROJO, 3).uno());
        assertEquals(1, j.cartasJugador("juan"));
    }

    @Test void testJugadorNoCantaUnoCuandoTieneUnaCarta() {
        Juego j = new Juego(mazoBasicoRojo, 2, "juan", "pedro");
        j.jugarCarta(new CartaNumero(Color.ROJO, 3));
        assertEquals(3, j.cartasJugador("juan"));
    }

    @Test void testJugadorCantaUnoCuandoTieneMasDeUnaCarta() {
        Juego j = new Juego(mazoBasicoRojo, 3, "juan", "pedro");
        j.jugarCarta(new CartaNumero(Color.ROJO, 3).uno());
        assertEquals(4, j.cartasJugador("juan"));
    }


    //tests cuando jugador gana

    @Test void testJugadorGanaTrasJugarTodasSusCartas() {
        Juego j = new Juego(mazoJugadorGana, 2, "juan", "pedro");
        j.jugarCarta(new CartaNumero(Color.ROJO, 3).uno()); // juan
        j.jugarCarta(new CartaNumero(Color.ROJO, 4).uno()); // pedro
        j.jugarCarta(new CartaNumero(Color.ROJO, 5)); // juan

        assertEquals(1, j.cantidadJugadoresEnJuego());
        assertFalse(j.contieneJugador("juan"));
        assertEquals("pedro", j.nombreJugadorDelTurno());
    }

    @Test void testJugadorGanaYSeSalteaEnLaRonda() {
        Juego j = new Juego(mazoJugadorGanaYSeSaltea, 2,
                "juan", "pedro", "luis");
        j.jugarCarta(new CartaNumero(Color.ROJO, 3).uno()); // juan
        j.jugarCarta(new CartaNumero(Color.ROJO, 4).uno()); // pedro
        j.jugarCarta(new CartaNumero(Color.ROJO, 5).uno()); // luis
        j.jugarCarta(new CartaNumero(Color.ROJO, 6)); // juan gana

        assertEquals(2, j.cantidadJugadoresEnJuego());
        assertFalse(j.contieneJugador("juan"));
        assertEquals("pedro", j.nombreJugadorDelTurno());
    }

    @Test void testVariosJugadoresGananYTurnoAvanzaCorrectamente() {
        Juego j = new Juego(mazoVariosJugadoresGanan, 2,
                "juan", "pedro", "luis", "carlos");

        j.jugarCarta(new CartaNumero(Color.ROJO, 3).uno()); // juan
        j.jugarCarta(new CartaNumero(Color.ROJO, 4).uno()); // pedro
        j.jugarCarta(new CartaNumero(Color.ROJO, 5).uno()); // luis
        j.jugarCarta(new CartaNumero(Color.ROJO, 6).uno()); // carlos
        j.jugarCarta(new CartaNumero(Color.ROJO, 7)); // juan (queda sin cartas)
        j.agarrarCartaMazo(); //pedro agarra carta porque no podía poner (tiene a2)
        j.jugarCarta(new CartaNumero(Color.ROJO, 6).uno()); //pedro juega la que agarró
        j.jugarCarta(new CartaNumero(Color.ROJO, 8)); // luis (queda sin cartas)

        assertEquals(2, j.cantidadJugadoresEnJuego());
        assertFalse(j.contieneJugador("juan"));
        assertFalse(j.contieneJugador("luis"));
        assertEquals("carlos", j.nombreJugadorDelTurno());
    }


    @Test void testPartidaFinalizadaCon2Jugadores() {
        Juego j = new Juego(mazoBasicoRojo, 2, "juan", "pedro");
        j.jugarCarta(new CartaNumero(Color.ROJO, 3).uno());  //juega juan
        j.jugarCarta(new CartaNumero(Color.ROJO, 4).uno());  //juega pedro
        j.jugarCarta(new CartaNumero(Color.ROJO, 5));  //juega juan
        assertEquals("pedro", j.nombreJugadorDelTurno());
        assertThrows(IllegalStateException.class, () -> j.jugarCarta(new CartaNumero(Color.ROJO, 7)), Juego.finishedGame);
    }

    //tests de fin del juego
    @Test void testPartidaFinalizadaCon3Jugadores() {
        Juego j = new Juego(mazoBasicoRojo, 2, "juan", "pedro", "luis");
        j.jugarCarta(new CartaNumero(Color.ROJO, 3).uno());
        j.jugarCarta(new CartaNumero(Color.ROJO, 4).uno());
        j.jugarCarta(new CartaNumero(Color.ROJO, 5).uno());
        j.jugarCarta(new CartaNumero(Color.ROJO, 6)); //gana juan
        j.jugarCarta(new CartaNumero(Color.ROJO, 7)); //gana pedro
        assertEquals("luis", j.nombreJugadorDelTurno());
        assertThrows(IllegalStateException.class, () -> j.jugarCarta(new CartaNumero(Color.ROJO, 8)), Juego.finishedGame);
    }

}


