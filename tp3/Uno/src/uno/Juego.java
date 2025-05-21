package uno;

import java.util.*;

public class Juego {
    private final Map<String, Jugador> jugadores = new LinkedHashMap<>();
    private Carta pozo;
    private final Deque<Carta> mazo;
    //private final List<String> orden = new ArrayList<>();  //tal vez no haga falta
    //private String jugadorActual;
    private int turno = 0;
    private int direccion = 1;
    private Carta cartaRecienRobada = null; //para que no juega carta diferente a la que agarra en ese turno

    public Juego(List<Carta> cartasIniciales, int cartasPorJugador, String... nombres) {
        inicializarJugadores(nombres);
        //jugadorActual = jugadores.keySet().iterator().next(); // primer jugador
        pozo = cartasIniciales.get(0);
        mazo = new ArrayDeque<>(cartasIniciales.subList(1, cartasIniciales.size()));
        repartir(cartasPorJugador);
        pozo.accionInicial(this);
    }

    private void inicializarJugadores(String... nombres) {
        for (String nombre : nombres) {
            jugadores.put(nombre, new Jugador(nombre));
            //orden.add(nombre);
        }
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
        Jugador j = jugadores.get(nombreJugadorDelTurno());

        if (cartaRecienRobada != null && carta.toString() == cartaRecienRobada.toString())
            throw new IllegalArgumentException("Solo se puede jugar la carta recién robada");

        if (!carta.puedeSerJugadoSobre(pozo))  throw new IllegalArgumentException("Jugada inválida");

        j.jugar(carta);
        pozo = carta;
        cartaRecienRobada = null;
        chequearSiTieneUnaCartaYCantaUno();
        chequearSiGanoJugador();
        pozo.accionSobre(this);
    }

    public void robar(int cantidadCartas) {
        for (int i = 0; i < cantidadCartas; i++) {
            chequearMazoQuedaVacio();
            jugadores.get(nombreJugadorDelTurno()).recibir(mazo.pop());
        }
    }

    public void agarrarCartaMazo() {
        Jugador j = jugadores.get(nombreJugadorDelTurno());
        if (j.tieneCartaJugable(pozo)) throw new IllegalStateException("No puede agarrar carta si tiene una jugable");
        chequearMazoQuedaVacio();
        cartaRecienRobada = mazo.pop();
        j.recibir(cartaRecienRobada);
    }

    public void avanzarTurno() {
        //turno = (turno + direccion + orden.size()) % orden.size();
        turno = (turno + direccion + jugadores.size()) % jugadores.size();
    }

    // se usa cuando hay Comodin en pozo inicial
    public void asignarColorAComodin(Color color) {pozo.asignarColor(color);}

    public void cambiarDireccion() { direccion *= -1; }

    private void chequearMazoQuedaVacio(){
        if (mazo.isEmpty()) throw new IllegalStateException("No hay más cartas");
    }

    private void chequearSiTieneUnaCartaYCantaUno() {
        Jugador j = jugadores.get(nombreJugadorDelTurno());
        if (j.tieneUnaCarta() && !pozo.fueCantadoUno()) { robar(2); } //carta en pozo fue jugada por el actual
        if (!j.tieneUnaCarta() && pozo.fueCantadoUno()) { throw new Error(); } // Cantar en mal momento -> tirar fallo
    }

    private void chequearSiGanoJugador() {
        String nombre = nombreJugadorDelTurno();
        Jugador j = jugadores.get(nombre);

        if (j.cantidad() == 0) {
            jugadores.remove(nombre);
            //orden.remove(nombre);
            // turno = (turno - 1 + orden.size()) % orden.size();
            turno = (turno - 1 + jugadores.size()) % jugadores.size();
        }
    }

    public Color colorPozo() { return pozo.color();}

    public int numPozo() { return pozo.numero(); }

    public String tipoCartaPozo() { return pozo.toString();}

    public int cartasJugador(String nombre) { return jugadores.get(nombre).cantidad();}

    public String nombreJugadorDelTurno() {
        //return orden.get(turno);
        return jugadores.keySet().stream().toList().get(turno);

    }

    public int cantidadJugadoresEnJuego() { return jugadores.size();}

    public boolean contieneJugador(String jugador) { return jugadores.containsKey(jugador); }

}

