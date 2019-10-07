package fi.dy.masa.minihud.renderer.shapes;

import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager.ShapeTypes;

public class ShapeDespawnSphere extends ShapeSpawnSphere
{
    public ShapeDespawnSphere()
    {
        super(ShapeTypes.DESPAWN_SPHERE, Configs.Colors.SHAPE_DESPAWN_SPHERE.getColor(), 128);
    }
}
