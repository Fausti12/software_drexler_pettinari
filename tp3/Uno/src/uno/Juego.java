package uno;

import java.util.*;


public class Juego {
    private final Map<String, Jugador> jugadores = new LinkedHashMap<>();
    private final Deque<Carta> pozo = new ArrayDeque<>();  //creo que con tener 1 carta funcionaría en todos los casos
    private final Iterator<String> turnos;
    private String actual;
    private final Deque<Carta> mazo;
    private Carta cartaRecienRobada = null;  //para evitar que se juegue carta diferente a la agarrada en ese turno
    private int direccion = 1; //para carta Reverse

    public Juego(List<Carta> cartasIniciales, String... nombres) {
        for (String nombre : nombres) jugadores.put(nombre, new Jugador(nombre));
        mazo = new ArrayDeque<>(cartasIniciales.subList(1, cartasIniciales.size()));
        pozo.push(cartasIniciales.get(0));  //se agrega primera carta de la lista al pozo

        repartir();
        turnos = ciclo(jugadores.keySet());
        avanzarTurno();
        // aplicar efecto de la primera carta del pozo si corresponde
        pozo.peek().accionSobre(this);
    }

    //para establecer una cantidad de cartas repartidas a cada jugador
    public Juego(List<Carta> cartasIniciales, int cartasPorJugador, String... nombres) {
        for (String nombre : nombres) jugadores.put(nombre, new Jugador(nombre));

        pozo.push(cartasIniciales.get(0));
        mazo = new ArrayDeque<>(cartasIniciales.subList(1, cartasIniciales.size()));

        repartir(cartasPorJugador);
        turnos = ciclo(jugadores.keySet());
        avanzarTurno();

        // aplicar efecto de la primera carta del pozo si corresponde
        pozo.peek().accionSobre(this);
    }

    private void repartir(int cartasPorJugador) {  //se reparte 1 carta a cada jugador (no las n cartas seguidas)
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

        if (cartaRecienRobada != null && carta != cartaRecienRobada)
            throw new IllegalArgumentException("Solo se puede jugar la carta recién robada");

        if (!carta.puedeSerJugadoSobre(pozo.peek()))
            throw new IllegalArgumentException("Jugada inválida");

        j.jugar(carta);
        pozo.push(carta);
        cartaRecienRobada = null; // se jugó la robada
        avanzarTurno();
        pozo.peek().accionSobre(this);

    }

    public void agarrarCartaMazo(){
        cartaRecienRobada = robar();
        jugadores.get(actual).recibir(cartaRecienRobada);
    }

    public void pasaTurno(){
        actual = turnos.next();
    }

    private void validarTurno(String nombre) {
        if (!actual.equals(nombre)) throw new IllegalArgumentException("No es el turno de " + nombre);
    }

    //private void avanzarTurno() {actual = turnos.next();}

    //public así lo usa Carta en aplicarEfecto
    public void avanzarTurno() {actual = turnos.next();}
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

    public String tipoCartaPozo(){ return pozo.peek().toString(); }

    public int cartasJugador(String nombre) {
        return jugadores.get(nombre).cantidad();
    }

    private Carta robar() {
        if (mazo.isEmpty()) throw new IllegalStateException("No hay más cartas");
        return mazo.pop();
    }

    //public así metodo de Carta puede usarlo
    public void robar(int cantidadCartas) { //usado para cuando aparece carta draw2
        if (mazo.isEmpty()) throw new IllegalStateException("No hay más cartas");
        for (int i = 0; i < cantidadCartas; i++){
            Carta carta = mazo.pop();
            jugadores.get(actual).recibir(carta);
        }
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
