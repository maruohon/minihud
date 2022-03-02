package fi.dy.masa.minihud.renderer;

import java.util.function.Consumer;
import org.lwjgl.opengl.GL11;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.block.entity.BlockEntityType;
import net.minecraft.block.entity.ConduitBlockEntity;
import net.minecraft.client.render.VertexFormat;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Matrix4f;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.LayerRange;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.ConduitExtra;
import fi.dy.masa.minihud.util.ShapeRenderType;
import fi.dy.masa.minihud.util.shape.SphereUtils;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

public class OverlayRendererConduitRange extends BaseBlockRangeOverlay<ConduitBlockEntity>
{
    public static final OverlayRendererConduitRange INSTANCE = new OverlayRendererConduitRange();

    public OverlayRendererConduitRange()
    {
        super(RendererToggle.OVERLAY_CONDUIT_RANGE, BlockEntityType.CONDUIT, ConduitBlockEntity.class);
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(VertexFormat.DrawMode.QUADS);
    }

    @Override
    protected void startBuffers()
    {
        BUFFER_1.begin(this.renderObjects.get(0).getGlMode(), VertexFormats.POSITION_COLOR);
    }

    @Override
    protected void uploadBuffers()
    {
        BUFFER_1.end();
        this.renderObjects.get(0).uploadData(BUFFER_1);
    }

    @Override
    public void draw(MatrixStack matrixStack, Matrix4f projMatrix)
    {
        this.preRender();

        this.renderObjects.get(0).draw(matrixStack, projMatrix);

        // Render the lines as quads with glPolygonMode(GL_LINE)
        RenderSystem.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_LINE);
        RenderSystem.disableBlend();
        this.renderObjects.get(0).draw(matrixStack, projMatrix);
        RenderSystem.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_FILL);
        RenderSystem.enableBlend();

        this.postRender();
    }

    @Override
    protected void renderBlockRange(World world, BlockPos pos, ConduitBlockEntity be, Vec3d cameraPos)
    {
        if (be.isActive() == false)
        {
            return;
        }

        int range = ((ConduitExtra) be).getStoredActivatingBlockCount() / 7 * 16;
        Color4f color = Configs.Colors.CONDUIT_RANGE_OVERLAY_COLOR.getColor();

        LongOpenHashSet positions = new LongOpenHashSet();
        Consumer<BlockPos.Mutable> positionCollector = (p) -> positions.add(p.asLong());
        SphereUtils.RingPositionTest test = this.getPositionTest(pos, range);

        SphereUtils.collectSpherePositions(positionCollector, test, pos, range);

        RenderUtils.renderCircleBlockPositions(positions, PositionUtils.ALL_DIRECTIONS,
                                               test, ShapeRenderType.OUTER_EDGE,
                                               new LayerRange(null), color, 0, cameraPos, BUFFER_1);
    }

    protected SphereUtils.RingPositionTest getPositionTest(BlockPos centerPos, int range)
    {
        Vec3d center = new Vec3d(centerPos.getX() + 0.5, centerPos.getY() + 0.5, centerPos.getZ() + 0.5);
        double squareRange = range * range;

        return (x, y, z, dir) -> SphereUtils.isPositionInsideOrClosestToRadiusOnBlockRing(
                x, y, z, center, squareRange, Direction.EAST);
    }
}
