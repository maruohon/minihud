package fi.dy.masa.minihud.renderer.shapes;

import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager.ShapeTypes;

public class ShapeCanSpawnSphere extends ShapeSpawnSphere
{
    public ShapeCanSpawnSphere()
    {
        super(ShapeTypes.CAN_SPAWN_SPHERE, Configs.Colors.SHAPE_CAN_SPAWN_SPHERE.getColor(), 24);
    }
}
