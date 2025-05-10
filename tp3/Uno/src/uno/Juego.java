package uno;

import java.util.*;


public class Juego {
    private final Map<String, Jugador> jugadores = new LinkedHashMap<>();
    private final Deque<Carta> pozo = new ArrayDeque<>();
    private final Iterator<String> turnos;
    private String actual;
    private final Deque<Carta> mazo;

    public Juego(List<Carta> cartasIniciales, String... nombres) {
        for (String nombre : nombres) jugadores.put(nombre, new Jugador(nombre));
        mazo = new ArrayDeque<>(cartasIniciales.subList(1, cartasIniciales.size()));
        pozo.push(cartasIniciales.get(0));  //se agrega primera carta de la lista al pozo

        repartir();
        turnos = ciclo(jugadores.keySet());
        actual = turnos.next();
    }

    //para establecer una cantidad de cartas repartidas a cada jugador
    public Juego(List<Carta> cartasIniciales, int cartasPorJugador, String... nombres) {
        for (String nombre : nombres) jugadores.put(nombre, new Jugador(nombre));

        pozo.push(cartasIniciales.get(0));
        mazo = new ArrayDeque<>(cartasIniciales.subList(1, cartasIniciales.size()));

        repartir(cartasPorJugador);
        turnos = ciclo(jugadores.keySet());
        actual = turnos.next();
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
        Jugador j = jugadores.get(actual);
        if (!carta.puedeSerJugadoSobre(pozo.peek())) throw new IllegalArgumentException("Jugada inválida");
        j.jugar(carta);
        pozo.push(carta);

        // if (j.tieneUnaCarta() && !carta.fueCantadoUno()) j.robar(this, 2);

        actual = turnos.next();
    }

    public void agarrarCartaMazo(){
        jugadores.get(actual).recibir(robar());
    }

    public void pasaTurno(){
        actual = turnos.next();
    }

    private void validarTurno(String nombre) {
        if (!actual.equals(nombre)) throw new IllegalArgumentException("No es el turno de " + nombre);
    }
/*
    public boolean cantoUno(String nombre) {
        return jugadores.get(nombre).cantoUno();
    }
*/
    public Color colorPozo() {
        return pozo.peek().color();
    }

    public int numPozo() {
        if (pozo.peek() instanceof CartaNumero c) return c.numero();
        throw new IllegalStateException("La carta del pozo no tiene número");
    }

    public int cartasJugador(String nombre) {
        return jugadores.get(nombre).cantidad();
    }

    private Carta robar() {
        if (mazo.isEmpty()) throw new IllegalStateException("No hay más cartas");
        return mazo.pop();
    }

    public String nombreJugadorDelTurno(){
        return actual;
    }

    private Iterator<String> ciclo(Set<String> nombres) {
        List<String> lista = new ArrayList<>(nombres);
        return new Iterator<>() {
            int i = 0;
            public boolean hasNext() { return true; }
            public String next() { return lista.get((i++) % lista.size()); }
        };
    }
}
