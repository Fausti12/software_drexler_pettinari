package uno;

import java.util.*;

public class Juego {
    private final Map<String, Jugador> jugadores = new LinkedHashMap<>();
    private final Deque<Carta> pozo = new ArrayDeque<>();
    private final Deque<Carta> mazo;
    private final List<String> orden = new ArrayList<>();
    private int turno = 0;
    private int direccion = 1;
    private Carta cartaRecienRobada = null; //para que no juega carta diferente a la que agarra en ese turno

    public Juego(List<Carta> cartasIniciales, String... nombres) {
        inicializarJugadores(nombres);
        mazo = new ArrayDeque<>(cartasIniciales.subList(1, cartasIniciales.size()));
        pozo.push(cartasIniciales.get(0));
        repartir();
        //avanzarTurno();
        pozo.peek().accionSobre(this);
    }

    public Juego(List<Carta> cartasIniciales, int cartasPorJugador, String... nombres) {
        inicializarJugadores(nombres);
        pozo.push(cartasIniciales.get(0));
        mazo = new ArrayDeque<>(cartasIniciales.subList(1, cartasIniciales.size()));
        repartir(cartasPorJugador);
        //avanzarTurno();
        pozo.peek().accionSobre(this);
    }

    private void inicializarJugadores(String... nombres) {
        for (String nombre : nombres) {
            jugadores.put(nombre, new Jugador(nombre));
            orden.add(nombre);
        }
    }

    private void repartir(int cartasPorJugador) {
        for (int i = 0; i < cartasPorJugador; i++) {
            for (Jugador j : jugadores.values()) {
                if (!mazo.isEmpty()) j.recibir(mazo.pop());
            }
        }
    }

    private void repartir() {
        while (!mazo.isEmpty()) {
            jugadores.values().forEach(j -> {
                if (!mazo.isEmpty()) j.recibir(mazo.pop());
            });
        }
    }

    public void jugarCarta(Carta carta) {
        Jugador j = jugadores.get(nombreJugadorDelTurno());

        if (cartaRecienRobada != null && carta != cartaRecienRobada)
            throw new IllegalArgumentException("Solo se puede jugar la carta recién robada");

        if (!carta.puedeSerJugadoSobre(pozo.peek()))
            throw new IllegalArgumentException("Jugada inválida");

        j.jugar(carta);
        pozo.push(carta);
        cartaRecienRobada = null;
        avanzarTurno();
        pozo.peek().accionSobre(this);
    }

    public void agarrarCartaMazo() {
        if (mazo.isEmpty()) throw new IllegalStateException("No hay más cartas");
        cartaRecienRobada = mazo.pop();
        jugadores.get(nombreJugadorDelTurno()).recibir(cartaRecienRobada);
    }

    public void avanzarTurno() {
        turno = (turno + direccion + orden.size()) % orden.size();
    }

    public void asignarColorAComodin(Color color) {
        pozo.peek().asignarColor(color);
    }

    public Color colorPozo() { return pozo.peek().color();}

    public int numPozo() {
        if (pozo.peek() instanceof CartaNumero c) return c.numero();
        throw new IllegalStateException("La carta del pozo no tiene número");
    }

    public String tipoCartaPozo() { return pozo.peek().toString();}

    public int cartasJugador(String nombre) { return jugadores.get(nombre).cantidad();}

    private Carta robar() {
        if (mazo.isEmpty()) throw new IllegalStateException("No hay más cartas");
        return mazo.pop();
    }

    public void robar(int cantidadCartas) {
        for (int i = 0; i < cantidadCartas; i++) {
            jugadores.get(nombreJugadorDelTurno()).recibir(robar());
        }
    }

    public String nombreJugadorDelTurno() { return orden.get(turno);}

    public void cambiarDireccion() { direccion *= -1; }
}

