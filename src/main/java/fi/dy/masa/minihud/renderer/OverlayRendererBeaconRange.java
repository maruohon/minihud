package fi.dy.masa.minihud.renderer;

import net.minecraft.block.entity.BeaconBlockEntity;
import net.minecraft.block.entity.BlockEntityType;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.mixin.IMixinBeaconBlockEntity;

public class OverlayRendererBeaconRange extends BaseBlockRangeOverlay<BeaconBlockEntity>
{
    public static final OverlayRendererBeaconRange INSTANCE = new OverlayRendererBeaconRange();

    public OverlayRendererBeaconRange()
    {
        super(RendererToggle.OVERLAY_BEACON_RANGE, BlockEntityType.BEACON, BeaconBlockEntity.class);
    }

    @Override
    protected void renderBlockRange(World world, BlockPos pos, BeaconBlockEntity be, Vec3d cameraPos)
    {
        int level = ((IMixinBeaconBlockEntity) be).minihud_getLevel();

        if (level >= 1 && level <= 4)
        {
            this.renderBeaconBox(world, pos, level, cameraPos, getColorForLevel(level));
        }
    }

    protected void renderBeaconBox(World world, BlockPos pos, int level, Vec3d cameraPos, Color4f color)
    {
        double x = pos.getX() - cameraPos.x;
        double y = pos.getY() - cameraPos.y;
        double z = pos.getZ() - cameraPos.z;

        int range = level * 10 + 10;
        double minX = x - range;
        double minY = y - range;
        double minZ = z - range;
        double maxX = x + range + 1;
        double maxY = this.getTopYOverTerrain(world, pos, range);
        double maxZ = z + range + 1;

        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllSidesBatchedQuads(minX, minY, minZ, maxX, maxY, maxZ, color, BUFFER_1);
        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllEdgesBatchedLines(minX, minY, minZ, maxX, maxY, maxZ, Color4f.fromColor(color, 1f), BUFFER_2);
    }

    public static Color4f getColorForLevel(int level)
    {
        switch (level)
        {
            case 1: return Configs.Colors.BEACON_RANGE_LVL1_OVERLAY_COLOR.getColor();
            case 2: return Configs.Colors.BEACON_RANGE_LVL2_OVERLAY_COLOR.getColor();
            case 3: return Configs.Colors.BEACON_RANGE_LVL3_OVERLAY_COLOR.getColor();
            default: return Configs.Colors.BEACON_RANGE_LVL4_OVERLAY_COLOR.getColor();
        }
    }
}
