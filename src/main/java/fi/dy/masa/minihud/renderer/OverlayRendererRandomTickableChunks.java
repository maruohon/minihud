package fi.dy.masa.minihud.renderer;

import java.util.HashSet;
import java.util.Set;
import javax.annotation.Nullable;
import com.google.gson.JsonObject;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;

public class OverlayRendererRandomTickableChunks extends OverlayRendererBase
{
    protected static boolean needsUpdate = true;
    @Nullable public static Vec3d newPos;
    private static final Direction[] HORIZONTALS = new Direction[] { Direction.NORTH, Direction.SOUTH, Direction.WEST, Direction.EAST };

    protected final RendererToggle toggle;
    protected Vec3d pos = Vec3d.ZERO;
    protected double minX;
    protected double minZ;
    protected double maxX;
    protected double maxZ;

    public static void setNeedsUpdate()
    {
        needsUpdate = true;
    }

    public OverlayRendererRandomTickableChunks(RendererToggle toggle)
    {
        this.toggle = toggle;
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return this.toggle.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        if (needsUpdate)
        {
            return true;
        }

        if (this.toggle == RendererToggle.OVERLAY_RANDOM_TICKS_FIXED)
        {
            return newPos != null;
        }
        // Player-following renderer
        else if (this.toggle == RendererToggle.OVERLAY_RANDOM_TICKS_PLAYER)
        {
            return entity.getX() != this.pos.x || entity.getZ() != this.pos.z;
        }

        return false;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        if (this.toggle == RendererToggle.OVERLAY_RANDOM_TICKS_PLAYER)
        {
            this.pos = entity.getPos();
        }
        else if (newPos != null)
        {
            this.pos = newPos;
            newPos = null;
        }

        final Color4f color = this.toggle == RendererToggle.OVERLAY_RANDOM_TICKS_PLAYER ?
                Configs.Colors.RANDOM_TICKS_PLAYER_OVERLAY_COLOR.getColor() :
                Configs.Colors.RANDOM_TICKS_FIXED_OVERLAY_COLOR.getColor();

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        Set<ChunkPos> chunks = this.getRandomTickableChunks(this.pos);

        for (ChunkPos pos : chunks)
        {
            this.renderChunkEdgesIfApplicable(cameraPos, pos, chunks, entity.getEntityWorld(), color);
        }

        BUFFER_1.end();
        BUFFER_2.end();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        needsUpdate = false;
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

    protected void renderChunkEdgesIfApplicable(Vec3d cameraPos, ChunkPos pos, Set<ChunkPos> chunks, World world, Color4f color)
    {
        for (Direction side : HORIZONTALS)
        {
            ChunkPos posAdj = new ChunkPos(pos.x + side.getOffsetX(), pos.z + side.getOffsetZ());

            if (chunks.contains(posAdj) == false)
            {
                this.renderChunkEdge(pos, side, cameraPos, color, world);
            }
        }
    }

    private void renderChunkEdge(ChunkPos pos, Direction side, Vec3d cameraPos, Color4f color, World world)
    {
        double minX, minZ, maxX, maxZ;

        switch (side)
        {
            case NORTH:
                minX = (double) (pos.x << 4);
                minZ = (double) (pos.z << 4);
                maxX = (double) (pos.x << 4) + 16.0;
                maxZ = (double) (pos.z << 4);
                break;
            case SOUTH:
                minX = (double) (pos.x << 4);
                minZ = (double) (pos.z << 4) + 16.0;
                maxX = (double) (pos.x << 4) + 16.0;
                maxZ = (double) (pos.z << 4) + 16.0;
                break;
            case WEST:
                minX = (double) (pos.x << 4);
                minZ = (double) (pos.z << 4);
                maxX = (double) (pos.x << 4);
                maxZ = (double) (pos.z << 4) + 16.0;
                break;
            case EAST:
                minX = (double) (pos.x << 4) + 16.0;
                minZ = (double) (pos.z << 4);
                maxX = (double) (pos.x << 4) + 16.0;
                maxZ = (double) (pos.z << 4) + 16.0;
                break;
            default:
                return;
        }

        int minY = world != null ? world.getBottomY() : -64;
        int maxY = world != null ? world.getTopY() : 320;

        RenderUtils.renderWallWithLines(minX, minY, minZ, maxX, maxY, maxZ, 16, 16, true, cameraPos, color, BUFFER_1, BUFFER_2);
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
