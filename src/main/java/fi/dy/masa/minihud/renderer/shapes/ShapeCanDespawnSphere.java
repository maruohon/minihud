package fi.dy.masa.minihud.renderer.shapes;

import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager.ShapeTypes;

public class ShapeCanDespawnSphere extends ShapeSpawnSphere
{
    public ShapeCanDespawnSphere()
    {
        super(ShapeTypes.CAN_DESPAWN_SPHERE, Configs.Colors.SHAPE_CAN_DESPAWN_SPHERE.getColor(), 32);
    }
}
