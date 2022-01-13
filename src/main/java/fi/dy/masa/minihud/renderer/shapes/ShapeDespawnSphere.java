package fi.dy.masa.minihud.renderer.shapes;

import fi.dy.masa.minihud.config.Configs;

public class ShapeDespawnSphere extends ShapeSpawnSphere
{
    public ShapeDespawnSphere()
    {
        super(ShapeType.DESPAWN_SPHERE, Configs.Colors.SHAPE_DESPAWN_SPHERE.getColor(), 128);

        this.setMargin(1.5);
    }
}
