package minihud.renderer.shapes;

import java.util.HashSet;

import net.minecraft.entity.Entity;

import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.position.BlockPos;
import malilib.util.position.Direction;
import malilib.util.position.Vec3d;
import minihud.config.Configs;

public class ShapeSphereBlocky extends ShapeCircleBase
{
    public ShapeSphereBlocky()
    {
        this(ShapeType.SPHERE_BLOCKY, Configs.Colors.SHAPE_SPHERE_BLOCKY.getColor(), 16);
    }

    public ShapeSphereBlocky(ShapeType type, Color4f color, double radius)
    {
        super(type, color, radius);
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        this.renderSphereShape(cameraPos);
        this.onPostUpdate(EntityWrap.getEntityPos(entity));
    }

    protected void renderSphereShape(Vec3d cameraPos)
    {
        BlockPos posCenter = this.getCenterBlock();
        BlockPos.MutBlockPos posMutable = new BlockPos.MutBlockPos();
        HashSet<BlockPos> spherePositions = new HashSet<>();

        this.startBuffers();

        //long before = System.nanoTime();
        posMutable.set(posCenter);
        this.addPositionsOnHorizontalRing(spherePositions, posMutable, Direction.EAST);

        posMutable.set(posCenter);
        this.addPositionsOnVerticalRing(spherePositions, posMutable, Direction.UP, Direction.EAST);

        final int r = (int) this.radius + 2;

        for (int i = 1; i < r; ++i)
        {
            // Horizontal rings
            posMutable.set(posCenter.getX(), posCenter.getY() - i, posCenter.getZ());
            this.addPositionsOnHorizontalRing(spherePositions, posMutable, Direction.EAST);

            posMutable.set(posCenter.getX(), posCenter.getY() + i, posCenter.getZ());
            this.addPositionsOnHorizontalRing(spherePositions, posMutable, Direction.EAST);

            // Vertical rings
            posMutable.set(posCenter.getX() - i, posCenter.getY(), posCenter.getZ());
            this.addPositionsOnVerticalRing(spherePositions, posMutable, Direction.UP, Direction.EAST);

            posMutable.set(posCenter.getX() + i, posCenter.getY(), posCenter.getZ());
            this.addPositionsOnVerticalRing(spherePositions, posMutable, Direction.UP, Direction.EAST);
        }
        //System.out.printf("time: %.6f s - margin: %.4f\n", (double) (System.nanoTime() - before) / 1000000000D, this.margin);
        //System.out.printf("spherePositions: %d\n", spherePositions.size());

        this.renderPositions(spherePositions, Direction.ALL_DIRECTIONS, this.mainAxis, this.color, cameraPos);
        //System.out.printf("rendered: %d\n", r);

        this.uploadBuffers();
    }
}
