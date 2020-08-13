package fi.dy.masa.minihud.renderer;

import java.util.Collection;
import org.lwjgl.opengl.GL11;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.WorldProvider;
import fi.dy.masa.malilib.render.overlay.BaseRenderObject;
import fi.dy.masa.malilib.util.data.Color4f;
import fi.dy.masa.malilib.util.data.IntBoundingBox;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.data.StructureData;
import fi.dy.masa.minihud.util.MiscUtils;
import fi.dy.masa.minihud.data.StructureType;

public class OverlayRendererStructures extends OverlayRendererBase
{
    public static OverlayRendererStructures instance;

    public OverlayRendererStructures()
    {
        instance = this;
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        if (RendererToggle.OVERLAY_STRUCTURE_MAIN_TOGGLE.isRendererEnabled() == false)
        {
            return false;
        }

        if (mc.world.provider.isSurfaceWorld())
        {
            return StructureType.DESERT_PYRAMID.isEnabled() ||
                   StructureType.IGLOO.isEnabled() ||
                   StructureType.JUNGLE_TEMPLE.isEnabled() ||
                   StructureType.MANSION.isEnabled() ||
                   StructureType.OCEAN_MONUMENT.isEnabled() ||
                   StructureType.STRONGHOLD.isEnabled() ||
                   StructureType.VILLAGE.isEnabled() ||
                   StructureType.WITCH_HUT.isEnabled();
        }
        else if (mc.world.provider.isNether())
        {
            return StructureType.NETHER_FORTRESS.isEnabled();
        }
        else
        {
            return StructureType.END_CITY.isEnabled();
        }
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        int hysteresis = 16;

        return DataStorage.getInstance().getStructureStorage().hasStructureDataChanged() ||
               Math.abs(entity.posX - this.lastUpdatePos.getX()) > hysteresis ||
               Math.abs(entity.posY - this.lastUpdatePos.getY()) > hysteresis ||
               Math.abs(entity.posZ - this.lastUpdatePos.getZ()) > hysteresis;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, Minecraft mc)
    {
        BaseRenderObject renderQuads = this.renderObjects.get(0);
        BaseRenderObject renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        this.updateStructures(mc.world.provider, this.lastUpdatePos, cameraPos, mc);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }

    private void updateStructures(WorldProvider provider, BlockPos playerPos, Vec3d cameraPos, Minecraft mc)
    {
        ArrayListMultimap<StructureType, StructureData> structures = DataStorage.getInstance().getStructureStorage().getCopyOfStructureData();
        int maxRange = (mc.gameSettings.renderDistanceChunks + 4) * 16;

        for (StructureType type : StructureType.values())
        {
            if (type.isEnabled() && type.existsInDimension(provider))
            {
                Collection<StructureData> structureData = structures.get(type);

                if (structureData.isEmpty() == false)
                {
                    this.renderStructuresWithinRange(type, structureData, playerPos, maxRange, cameraPos);
                }
            }
        }
    }

    private void renderStructuresWithinRange(StructureType type, Collection<StructureData> structureData, BlockPos playerPos, int maxRange, Vec3d cameraPos)
    {
        for (StructureData structure : structureData)
        {
            if (MiscUtils.isStructureWithinRange(structure.getBoundingBox(), playerPos, maxRange))
            {
                this.renderStructure(type, structure, cameraPos);
            }
        }
    }

    private void renderStructure(StructureType type, StructureData structure, Vec3d cameraPos)
    {
        Color4f color = type.getToggle().getColorMain().getColor();
        ImmutableList<IntBoundingBox> components = structure.getComponents();

        fi.dy.masa.malilib.render.RenderUtils.drawBox(structure.getBoundingBox(), cameraPos, color, BUFFER_1, BUFFER_2);

        if (components.isEmpty() == false)
        {
            if (components.size() > 1 || components.get(0).equals(structure.getBoundingBox()) == false)
            {
                color = type.getToggle().getColorComponents().getColor();

                for (IntBoundingBox bb : components)
                {
                    fi.dy.masa.malilib.render.RenderUtils.drawBox(bb, cameraPos, color, BUFFER_1, BUFFER_2);
                }
            }
        }
    }
}
