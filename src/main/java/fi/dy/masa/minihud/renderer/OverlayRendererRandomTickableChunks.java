package fi.dy.masa.minihud.renderer;

import java.util.HashSet;
import java.util.Set;
import javax.annotation.Nullable;
import org.lwjgl.opengl.GL11;
import com.google.gson.JsonObject;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.Vec3d;

public class OverlayRendererRandomTickableChunks extends OverlayRendererBase
{
    @Nullable public static Vec3d newPos;
    private static final EnumFacing[] HORIZONTALS = new EnumFacing[] { EnumFacing.NORTH, EnumFacing.SOUTH, EnumFacing.WEST, EnumFacing.EAST };

    protected final RendererToggle toggle;
    protected Vec3d pos = Vec3d.ZERO;

    public OverlayRendererRandomTickableChunks(RendererToggle toggle)
    {
        this.toggle = toggle;
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return this.toggle.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        if (this.toggle == RendererToggle.OVERLAY_RANDOM_TICKS_FIXED)
        {
            return newPos != null;
        }
        // Player-following renderer
        else if (this.toggle == RendererToggle.OVERLAY_RANDOM_TICKS_PLAYER)
        {
            return entity.posX != this.pos.x || entity.posZ != this.pos.z;
        }

        return false;
    }

    @Override
    public void update(Entity entity, Minecraft mc)
    {
        if (this.toggle == RendererToggle.OVERLAY_RANDOM_TICKS_PLAYER)
        {
            this.pos = entity.getPositionVector();
        }
        else if (newPos != null)
        {
            this.pos = newPos;
            newPos = null;
        }

        this.setPosition(new BlockPos(this.pos));

        final int color = this.toggle == RendererToggle.OVERLAY_RANDOM_TICKS_PLAYER ?
                Configs.Colors.RANDOM_TICKS_PLAYER_OVERLAY_COLOR.getIntegerValue() :
                Configs.Colors.RANDOM_TICKS_FIXED_OVERLAY_COLOR.getIntegerValue();

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        Set<ChunkPos> chunks = this.getRandomTickableChunks(this.pos);

        for (ChunkPos pos : chunks)
        {
            this.renderChunkEdgesIfApplicable(pos, chunks, entity, color);
        }

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);
    }

    protected Set<ChunkPos> getRandomTickableChunks(Vec3d posCenter)
    {
        Set<ChunkPos> set = new HashSet<>();
        final int centerChunkX = ((int) Math.floor(posCenter.x)) >> 4;
        final int centerChunkZ = ((int) Math.floor(posCenter.z)) >> 4;
        final double maxRange = 128D * 128D;
        final int r = 9;

        for (int cz = centerChunkZ - r; cz <= centerChunkZ + r; ++cz)
        {
            for (int cx = centerChunkX - r; cx <= centerChunkX + r; ++cx)
            {
                double dx = (double) (cx * 16 + 8) - posCenter.x;
                double dz = (double) (cz * 16 + 8) - posCenter.z;

                if ((dx * dx + dz * dz) < maxRange)
                {
                    set.add(new ChunkPos(cx, cz));
                }
            }
        }

        return set;
    }

    protected void renderChunkEdgesIfApplicable(ChunkPos pos, Set<ChunkPos> chunks, Entity entity, int color)
    {
        for (EnumFacing side : HORIZONTALS)
        {
            ChunkPos posTmp = new ChunkPos(pos.x + side.getXOffset(), pos.z + side.getZOffset());

            if (chunks.contains(posTmp) == false)
            {
                RenderUtils.renderVerticalWallsOfLinesWithinRange(BUFFER_1, BUFFER_2,
                        side.getAxis(), this.getStartPos(pos, side), this.getEndPos(pos, side),
                        512, 256, 16, 16, entity, color);
            }
        }
    }

    protected BlockPos getStartPos(ChunkPos chunkPos, EnumFacing side)
    {
        switch (side)
        {
            case NORTH:     return new BlockPos( chunkPos.x << 4      , 0,  chunkPos.z << 4      );
            case SOUTH:     return new BlockPos( chunkPos.x << 4      , 0, (chunkPos.z << 4) + 16);
            case WEST:      return new BlockPos( chunkPos.x << 4      , 0,  chunkPos.z << 4      );
            case EAST:      return new BlockPos((chunkPos.x << 4) + 16, 0,  chunkPos.z << 4      );
            default:
        }

        return BlockPos.ORIGIN;
    }

    protected BlockPos getEndPos(ChunkPos chunkPos, EnumFacing side)
    {
        switch (side)
        {
            case NORTH:     return new BlockPos((chunkPos.x << 4) + 16, 256,  chunkPos.z << 4      );
            case SOUTH:     return new BlockPos((chunkPos.x << 4) + 16, 256, (chunkPos.z << 4) + 16);
            case WEST:      return new BlockPos( chunkPos.x << 4      , 256, (chunkPos.z << 4) + 16);
            case EAST:      return new BlockPos((chunkPos.x << 4) + 16, 256, (chunkPos.z << 4) + 16);
            default:
        }

        return BlockPos.ORIGIN;
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }

    @Override
    public String getSaveId()
    {
        return this.toggle == RendererToggle.OVERLAY_RANDOM_TICKS_FIXED ? "random_tickable_chunks" : "";
    }

    @Nullable
    @Override
    public JsonObject toJson()
    {
        JsonObject obj = new JsonObject();
        obj.add("pos", JsonUtils.vec3dToJson(this.pos));
        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        Vec3d pos = JsonUtils.vec3dFromJson(obj, "pos");

        if (pos != null && this.toggle == RendererToggle.OVERLAY_RANDOM_TICKS_FIXED)
        {
            newPos = pos;
        }
    }
}
