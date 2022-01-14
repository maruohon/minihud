package fi.dy.masa.minihud.renderer.shapes;

import java.util.function.Supplier;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.util.StringUtils;

public enum ShapeType
{
    BOX                     ("box",                     "minihud.label.shapes.box",                     ShapeBox::new),
    CIRCLE                  ("circle",                  "minihud.label.shapes.circle",                  ShapeCircle::new),
    BLOCK_LINE              ("block_line",              "minihud.label.shapes.block_line",              ShapeLineBlock::new),
    SPHERE_BLOCKY           ("sphere_blocky",           "minihud.label.shapes.sphere_blocky",           ShapeSphereBlocky::new),
    ADJUSTABLE_SPAWN_SPHERE ("adjustable_spawn_sphere", "minihud.label.shapes.adjustable_spawn_sphere", ShapeSpawnSphere::new),
    CAN_SPAWN_SPHERE        ("can_spawn_sphere",        "minihud.label.shapes.can_spawn_sphere",        ShapeCanSpawnSphere::new),
    CAN_DESPAWN_SPHERE      ("can_despawn_sphere",      "minihud.label.shapes.can_despawn_sphere",      ShapeCanDespawnSphere::new),
    DESPAWN_SPHERE          ("despawn_sphere",          "minihud.label.shapes.despawn_sphere",          ShapeDespawnSphere::new);

    public static final ImmutableList<ShapeType> VALUES = ImmutableList.copyOf(values());

    private final String id;
    private final String translationKey;
    private final Supplier<ShapeBase> shapeFactory;

    ShapeType(String id, String translationKey, Supplier<ShapeBase> shapeFactory)
    {
        this.id = id;
        this.translationKey = translationKey;
        this.shapeFactory = shapeFactory;
    }

    public String getId()
    {
        return this.id;
    }

    public String getDisplayName()
    {
        return StringUtils.translate(this.translationKey);
    }

    public ShapeBase createShape()
    {
        return this.shapeFactory.get();
    }

    @Nullable
    public static ShapeType fromString(String id)
    {
        for (ShapeType type : ShapeType.VALUES)
        {
            if (type.getId().equalsIgnoreCase(id))
            {
                return type;
            }
        }

        return null;
    }
}
