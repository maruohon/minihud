package minihud.renderer.shapes;

import minihud.config.Configs;

public class ShapeCanSpawnSphere extends ShapeSpawnSphere
{
    public ShapeCanSpawnSphere()
    {
        super(ShapeType.CAN_SPAWN_SPHERE, Configs.Colors.SHAPE_CAN_SPAWN_SPHERE.getColor(), 24);
    }
}
