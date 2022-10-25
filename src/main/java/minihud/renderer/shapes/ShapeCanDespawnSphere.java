package minihud.renderer.shapes;

import minihud.config.Configs;

public class ShapeCanDespawnSphere extends ShapeSpawnSphere
{
    public ShapeCanDespawnSphere()
    {
        super(ShapeType.CAN_DESPAWN_SPHERE, Configs.Colors.SHAPE_CAN_DESPAWN_SPHERE.getColor(), 32);
    }
}
