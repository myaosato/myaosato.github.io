class Thing {
    public Thing(String name) {
        this.name = name;
    }
    private String name = "Thing";
    public String getName() {
        return this.name;
    }   
}
class Asteroid extends Thing{
    public Asteroid(String name) {
        super(name);
    }
    public void collide_with(Asteroid other) {
        System.out.println(String.format("A: %s - A: %s", this.getName(), other.getName()));
    }
    public void collide_with(Spaceship other){
        System.out.println(String.format("A: %s - S: %s", this.getName(), other.getName()));
    }
}
 
class Spaceship extends Thing{
    public Spaceship(String name) {
        super(name);
    }
    public void collide_with(Asteroid other) {
        System.out.println(String.format("S: %s - A: %s", this.getName(), other.getName()));
    }
    public void collide_with(Spaceship other){
        System.out.println(String.format("S: %s - S: %s", this.getName(), other.getName()));
    }
}

class MultipleDispatch {
    public static void main(String args[]){
        Asteroid asteroid = new Asteroid("Ceres");
        Spaceship spaceship = new Spaceship("Yamato");
        asteroid.collide_with(asteroid);
        asteroid.collide_with(spaceship);
        spaceship.collide_with(asteroid);
        spaceship.collide_with(spaceship);
    }
}
        
