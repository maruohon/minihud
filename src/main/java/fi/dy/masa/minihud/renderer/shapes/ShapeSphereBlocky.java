package fi.dy.masa.minihud.renderer.shapes;

import java.util.List;
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
import fi.dy.masa.minihud.renderer.RenderUtils;
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
        this.needsUpdate = false;
    }

    protected SphereUtils.RingPositionTest getPositionTest()
    {
        return (x, y, z, dir) -> SphereUtils.isPositionInsideOrClosestToRadiusOnBlockRing(
                                    x, y, z, this.getEffectiveCenter(), this.getSquaredRadius(), Direction.EAST);
    }

    protected double getTotalRadius()
    {
        return this.getRadius();
    }

    protected void renderSphereShape(Vec3d cameraPos)
    {
        SphereUtils.RingPositionTest test = this.getPositionTest();
        LongOpenHashSet positions = this.collectSpherePositions(test);
        double expand = 0;

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);

        if (this.getCombineQuads())
        {
            List<SideQuad> quads = SphereUtils.buildSphereShellToQuads(positions, this.mainAxis.getAxis(),
                                                                       test, this.renderType, this.layerRange);
            RenderUtils.renderQuads(quads, this.color, expand, cameraPos, BUFFER_1);
        }
        else
        {
            RenderUtils.renderCircleBlockPositions(positions, PositionUtils.ALL_DIRECTIONS, test, this.renderType,
                                                   this.layerRange, this.color, expand, cameraPos, BUFFER_1);
        }

        BUFFER_1.end();
        renderQuads.uploadData(BUFFER_1);
    }

    protected LongOpenHashSet collectSpherePositions(SphereUtils.RingPositionTest test)
    {
        LongOpenHashSet positions = new LongOpenHashSet();
        BlockPos posCenter = this.getCenterBlock();
        BlockPos.Mutable mutablePos = new BlockPos.Mutable();
        Consumer<BlockPos.Mutable> positionConsumer = this.getPositionCollector(positions);

        //long before = System.nanoTime();
        mutablePos.set(posCenter);
        SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test);

        mutablePos.set(posCenter);
        SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, Direction.NORTH, test);

        final int r = (int) this.getTotalRadius() + 2;

        for (int i = 1; i < r; ++i)
        {
            // Horizontal rings
            mutablePos.set(posCenter.getX(), posCenter.getY() - i, posCenter.getZ());
            SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test);

            mutablePos.set(posCenter.getX(), posCenter.getY() + i, posCenter.getZ());
            SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test);

            // Vertical rings
            mutablePos.set(posCenter.getX(), posCenter.getY(), posCenter.getZ() - i);
            SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, Direction.NORTH, test);

            mutablePos.set(posCenter.getX(), posCenter.getY(), posCenter.getZ() + i);
            SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, Direction.NORTH, test);
        }
        //System.out.printf("time: %.6f s\n", (double) (System.nanoTime() - before) / 1000000000D);

        return positions;
    }
}
