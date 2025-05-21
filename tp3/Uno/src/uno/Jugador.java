package uno;

import java.util.ArrayList;
import java.util.List;

public class Jugador {
    private String nombre;
    private final List<Carta> mano = new ArrayList<>();
    public Jugador(String nombre) {this.nombre = nombre;}

    public void recibir(Carta carta) { mano.add(carta); }

    public void jugar(Carta carta) {
        Carta cartaDelMazo = mano.stream().filter(c -> c.toString().equals(carta.toString())).findFirst().orElse(null);
        if ( cartaDelMazo == null ) {
            throw new IllegalArgumentException("La carta no está en la mano");
        }
        mano.remove(cartaDelMazo);
    }

    public boolean tieneCartaJugable(Carta pozo) {
        return mano.stream().anyMatch(carta -> carta.puedeSerJugadoSobre(pozo));
    }

    public boolean tieneUnaCarta() { return mano.size() == 1; }

    public int cantidad() {
        return mano.size();
    }
}
