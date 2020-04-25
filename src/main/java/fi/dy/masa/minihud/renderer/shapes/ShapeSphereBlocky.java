package fi.dy.masa.minihud.renderer.shapes;

import java.util.HashSet;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.RenderObjectBase;

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
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        this.renderSphereShape(cameraPos);
        this.onPostUpdate(entity.getPos());
    }

    protected void renderSphereShape(Vec3d cameraPos)
    {
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);

        BlockPos posCenter = this.getCenterBlock();
        BlockPos.Mutable posMutable = new BlockPos.Mutable();
        HashSet<BlockPos> spherePositions = new HashSet<>();

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

        Direction[] sides = FACING_ALL;

        this.renderPositions(spherePositions, sides, this.mainAxis, this.color, cameraPos);
        //System.out.printf("rendered: %d\n", r);

        BUFFER_1.end();

        renderQuads.uploadData(BUFFER_1);
    }
}
