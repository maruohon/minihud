package fi.dy.masa.minihud.renderer.shapes;

import java.util.function.Consumer;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.RenderObjectBase;
import fi.dy.masa.minihud.util.shape.SphereUtils;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

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

    protected SphereUtils.RingPositionTest getPositionTest()
    {
        return (x, y, z, dir) -> SphereUtils.isPositionInsideOrClosestToRadiusOnBlockRing(
                                    x, y, z, this.effectiveCenter, this.radiusSq, Direction.EAST);
    }

    protected void renderSphereShape(Vec3d cameraPos)
    {
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);

        SphereUtils.RingPositionTest test = this.getPositionTest();
        LongOpenHashSet positions = new LongOpenHashSet();

        this.collectSpherePositions(positions, test);
        //System.out.printf("time: %.6f s - margin: %.4f\n", (double) (System.nanoTime() - before) / 1000000000D, this.margin);
        //System.out.printf("spherePositions: %d\n", spherePositions.size());

        this.renderPositions(positions, PositionUtils.ALL_DIRECTIONS, test, this.color, 0, cameraPos);
        //System.out.printf("rendered: %d\n", r);

        BUFFER_1.end();

        renderQuads.uploadData(BUFFER_1);
    }

    protected void collectSpherePositions(LongOpenHashSet positions, SphereUtils.RingPositionTest test)
    {
        BlockPos posCenter = this.getCenterBlock();
        BlockPos.Mutable mutablePos = new BlockPos.Mutable();
        Consumer<BlockPos.Mutable> positionConsumer = this.getPositionCollector(positions);

        //long before = System.nanoTime();
        mutablePos.set(posCenter);
        SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test, this.radius);

        mutablePos.set(posCenter);
        SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, Direction.NORTH, test, this.radius);

        final int r = (int) this.radius + 2;

        for (int i = 1; i < r; ++i)
        {
            // Horizontal rings
            mutablePos.set(posCenter.getX(), posCenter.getY() - i, posCenter.getZ());
            SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test, this.radius);

            mutablePos.set(posCenter.getX(), posCenter.getY() + i, posCenter.getZ());
            SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test, this.radius);

            // Vertical rings
            mutablePos.set(posCenter.getX(), posCenter.getY(), posCenter.getZ() - i);
            SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, Direction.NORTH, test, this.radius);

            mutablePos.set(posCenter.getX(), posCenter.getY(), posCenter.getZ() + i);
            SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, Direction.NORTH, test, this.radius);
        }
    }
}
