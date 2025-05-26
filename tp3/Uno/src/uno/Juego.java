package uno;

import java.util.*;

public class Juego {
    private final Map<String, Jugador> jugadores = new LinkedHashMap<>();
    private Carta pozo;
    private final Deque<Carta> mazo;
    private Controlador controlador = new ControladorIzquierda(); // por default la ronda va hacia la izquierda
    private Jugador jugadorActual;
    private boolean juegoFinalizado = false;

    // mensajes de error
    public static String cardNotInHand = "La carta no está en la mano";
    public static String invalidPlay = "Jugada inválida";
    public static String useYourPlayableCard = "No puede agarrar carta si tiene una jugable";
    public static String noMoreCards = "No hay más cartas";
    public static String finishedGame = "Juego finalizado";

    public Juego(List<Carta> cartasIniciales, int cartasPorJugador, String... nombres) {
        inicializarJugadores(nombres);
        pozo = cartasIniciales.get(0);
        mazo = new ArrayDeque<>(cartasIniciales.subList(1, cartasIniciales.size()));
        repartir(cartasPorJugador);
        pozo.accionInicial(this);
    }

    private void inicializarJugadores(String... nombres) {
        crearJugadores(nombres);
        enlazarJugadoresCircularmente(nombres);
        jugadorActual = jugadores.get(nombres[0]);
    }

    private void crearJugadores(String[] nombres) {
        Arrays.stream(nombres).forEach(nombre -> jugadores.put(nombre, new Jugador(nombre)));
    }

    private void enlazarJugadoresCircularmente(String[] nombres) {
        for (int i = 0; i < nombres.length; i++) {
            Jugador actual = jugadores.get(nombres[i]);
            Jugador izquierda = jugadores.get(nombres[(i + 1) % nombres.length]);                // A la izquierda está el jugador siguiente
            Jugador derecha = jugadores.get(nombres[(i - 1 + nombres.length) % nombres.length]); // A la derecha está el jugador anterior
            actual.asignarIzquierda(izquierda);
            actual.asignarDerecha(derecha);
        }
    }


    private void repartir(int cartasPorJugador) {
        for (int i = 0; i < cartasPorJugador; i++) {
            jugadores.values().forEach(j -> {
                chequearMazoQuedaVacio();
                j.recibir(mazo.pop());
            });
        }
    }


    public void jugarCarta(Carta carta) {
        verificarJuegoNoFinalizado();
        if (!jugadorActual.tieneLaCarta(carta))  throw new IllegalArgumentException(cardNotInHand);
        if (!carta.puedeSerJugadoSobre(pozo))  throw new IllegalArgumentException(invalidPlay);
        jugadorActual.jugar(carta);
        pozo = carta;
        chequearSiTieneUnaCartaYCantaUno();
        chequearSiGanoJugador();
        pozo.accionSobre(this);
    }

    public void robar(int cantidadCartas) {
        for (int i = 0; i < cantidadCartas; i++) {
            chequearMazoQuedaVacio();
            jugadorActual.recibir(mazo.pop());
        }
    }

    public void agarrarCartaMazo() {
        verificarJuegoNoFinalizado();
        if (jugadorActual.tieneCartaJugable(pozo)) throw new IllegalStateException(useYourPlayableCard);
        chequearMazoQuedaVacio();
        jugadorActual.recibir(mazo.pop());
    }

    public void avanzarTurno() {
        jugadorActual = controlador.jugadorSiguiente(jugadorActual);
    }

    // se usa cuando hay Comodin en pozo inicial
    public void asignarColorAComodin(Color color) {pozo.asignarColor(color);}

    public void cambiarDireccion() {
        controlador = controlador.opuesto();
    }

    private void chequearMazoQuedaVacio(){
        if (mazo.isEmpty()) throw new IllegalStateException(noMoreCards);
    }

    private void chequearSiTieneUnaCartaYCantaUno() {
        if (jugadorActual.tieneUnaCarta() ^ pozo.fueCantadoUno()) { robar(2); }
    }

    private void chequearSiGanoJugador() {
        if (jugadorActual.cantidad() == 0) {
            jugadores.remove(jugadorActual.getNombre());
            jugadorActual.getDerecha().asignarIzquierda(jugadorActual.getIzquierda()); // al jugador que tengo a la derecha, le asigno el jugador que tengo a MI izquierda como SU izquierda
            jugadorActual.getIzquierda().asignarDerecha(jugadorActual.getDerecha());   // lo mismo que antes, pero al revés
        }
        if (jugadores.size() == 1) { juegoFinalizado = true; }
    }

    private void verificarJuegoNoFinalizado() {
        if (juegoFinalizado) throw new IllegalStateException(finishedGame);
    }

    public Color colorPozo() { return pozo.color();}

    public int numPozo() { return pozo.numero(); }

    public String tipoCartaPozo() { return pozo.toString();}

    public int cartasJugador(String nombre) { return jugadores.get(nombre).cantidad();}

    public String nombreJugadorDelTurno() { return jugadorActual.getNombre(); }

    public int cantidadJugadoresEnJuego() { return jugadores.size();}

    public boolean contieneJugador(String jugador) { return jugadores.containsKey(jugador); }

}

