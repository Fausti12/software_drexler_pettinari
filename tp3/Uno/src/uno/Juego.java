package uno;

import java.util.*;

public class Juego {
    private final Map<String, Jugador> jugadores = new LinkedHashMap<>();
    private Carta pozo;
    private final Deque<Carta> mazo;
    private Controlador controlador = new ControladorIzquierda(); // por default la ronda va hacia la izquierda
    private Jugador jugadorActual;
    private boolean juegoFinalizado = false;


    public Juego(List<Carta> cartasIniciales, int cartasPorJugador, String... nombres) {
        inicializarJugadores(nombres);
        pozo = cartasIniciales.get(0);
        mazo = new ArrayDeque<>(cartasIniciales.subList(1, cartasIniciales.size()));
        repartir(cartasPorJugador);
        pozo.accionInicial(this);
    }

    private void inicializarJugadores(String... nombres) {
        Jugador firstPlayer = new Jugador(nombres[0]);
        jugadorActual = firstPlayer;
        jugadores.put(nombres[0], firstPlayer);
        for (int i = 1; i < nombres.length; i++) {
            Jugador player = new Jugador(nombres[i]);
            jugadores.put(nombres[i], player);
            // La ronda comienza yendo a la izquierda
            player.asignarDerecha(jugadores.get(nombres[i-1]));    // A la derecha está el jugador anterior
            jugadores.get(nombres[i-1]).asignarIzquierda(player);  // Me pongo a la izquierda del jugador anterior
        }
        jugadores.get(nombres[0]).asignarDerecha(jugadores.get(nombres[nombres.length-1]));
        jugadores.get(nombres[nombres.length-1]).asignarIzquierda(jugadores.get(nombres[0]));
    }

    private void repartir(int cartasPorJugador) {
        for (int i = 0; i < cartasPorJugador; i++) {
            for (Jugador j : jugadores.values()) {
                chequearMazoQuedaVacio();
                j.recibir(mazo.pop());
            }
        }
    }

    public void jugarCarta(Carta carta) {
        verificarJuegoNoFinalizado();
        if (!carta.puedeSerJugadoSobre(pozo))  throw new IllegalArgumentException("Jugada inválida");
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
        if (jugadorActual.tieneCartaJugable(pozo)) throw new IllegalStateException("No puede agarrar carta si tiene una jugable");
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
        if (mazo.isEmpty()) throw new IllegalStateException("No hay más cartas");
    }

    private void chequearSiTieneUnaCartaYCantaUno() {
        if (jugadorActual.tieneUnaCarta() && !pozo.fueCantadoUno()) { robar(2); } //carta en pozo fue jugada por el actual
        if (!jugadorActual.tieneUnaCarta() && pozo.fueCantadoUno()) { robar(2); } // Cantar en mal momento -> tirar fallo
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
        if (juegoFinalizado) throw new IllegalStateException("El juego ya terminó");
    }

    public Color colorPozo() { return pozo.color();}

    public int numPozo() { return pozo.numero(); }

    public String tipoCartaPozo() { return pozo.toString();}

    public int cartasJugador(String nombre) { return jugadores.get(nombre).cantidad();}

    public String nombreJugadorDelTurno() { return jugadorActual.getNombre(); }

    public int cantidadJugadoresEnJuego() { return jugadores.size();}

    public boolean contieneJugador(String jugador) { return jugadores.containsKey(jugador); }

}

