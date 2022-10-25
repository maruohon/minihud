package minihud.renderer.shapes;

import minihud.config.Configs;

public class ShapeDespawnSphere extends ShapeSpawnSphere
{
    public ShapeDespawnSphere()
    {
        super(ShapeType.DESPAWN_SPHERE, Configs.Colors.SHAPE_DESPAWN_SPHERE.getColor(), 128);
    }
}
