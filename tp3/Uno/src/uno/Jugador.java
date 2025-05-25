package uno;

import java.util.ArrayList;
import java.util.List;

public class Jugador {
    private final List<Carta> mano = new ArrayList<>();
    private String nombre;
    private Jugador izquierda = null;
    private Jugador derecha = null;

    public Jugador(String nombre) {
        this.nombre = nombre;
    }

    public void recibir(Carta carta) { mano.add(carta); }

    public void jugar(Carta carta) {
        Carta cartaDelMazo = mano.stream().filter(c -> c.toString().equals(carta.toString())).findFirst().orElse(null);
        mano.remove(cartaDelMazo);
    }

    public boolean tieneLaCarta(Carta carta) {
        return mano.stream().anyMatch(c -> c.toString().equals(carta.toString()));
    }

    public boolean tieneCartaJugable(Carta pozo) {
        return mano.stream().anyMatch(carta -> carta.puedeSerJugadoSobre(pozo));
    }

    public void asignarIzquierda(Jugador player){
        izquierda = player;
    }
    public void asignarDerecha(Jugador player){
        derecha = player;
    }

    public Jugador getIzquierda() {
        return izquierda;
    }

    public Jugador getDerecha() {
        return derecha;
    }

    public boolean tieneUnaCarta() { return mano.size() == 1; }

    public int cantidad() {
        return mano.size();
    }

    public String getNombre() {
        return nombre;
    }

}
