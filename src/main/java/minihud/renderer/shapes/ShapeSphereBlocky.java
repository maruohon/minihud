package minihud.renderer.shapes;

import java.util.HashSet;

import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;

import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
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
        BlockPos.MutableBlockPos posMutable = new BlockPos.MutableBlockPos();
        HashSet<BlockPos> spherePositions = new HashSet<>();

        this.startBuffers();

        //long before = System.nanoTime();
        posMutable.setPos(posCenter);
        this.addPositionsOnHorizontalRing(spherePositions, posMutable, EnumFacing.EAST);

        posMutable.setPos(posCenter);
        this.addPositionsOnVerticalRing(spherePositions, posMutable, EnumFacing.UP, EnumFacing.EAST);

        final int r = (int) this.radius + 2;

        for (int i = 1; i < r; ++i)
        {
            // Horizontal rings
            posMutable.setPos(posCenter.getX(), posCenter.getY() - i, posCenter.getZ());
            this.addPositionsOnHorizontalRing(spherePositions, posMutable, EnumFacing.EAST);

            posMutable.setPos(posCenter.getX(), posCenter.getY() + i, posCenter.getZ());
            this.addPositionsOnHorizontalRing(spherePositions, posMutable, EnumFacing.EAST);

            // Vertical rings
            posMutable.setPos(posCenter.getX() - i, posCenter.getY(), posCenter.getZ());
            this.addPositionsOnVerticalRing(spherePositions, posMutable, EnumFacing.UP, EnumFacing.EAST);

            posMutable.setPos(posCenter.getX() + i, posCenter.getY(), posCenter.getZ());
            this.addPositionsOnVerticalRing(spherePositions, posMutable, EnumFacing.UP, EnumFacing.EAST);
        }
        //System.out.printf("time: %.6f s - margin: %.4f\n", (double) (System.nanoTime() - before) / 1000000000D, this.margin);
        //System.out.printf("spherePositions: %d\n", spherePositions.size());

        EnumFacing[] sides = FACING_ALL;

        this.renderPositions(spherePositions, sides, this.mainAxis, this.color, cameraPos);
        //System.out.printf("rendered: %d\n", r);

        this.uploadBuffers();
    }
}
