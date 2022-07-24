package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.IntBoundingBox;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;
import fi.dy.masa.minihud.util.DataStorage;
import fi.dy.masa.minihud.util.MiscUtils;
import fi.dy.masa.minihud.util.StructureData;
import fi.dy.masa.minihud.util.StructureType;

public class OverlayRendererStructures extends OverlayRendererBase
{
    public static final OverlayRendererStructures INSTANCE = new OverlayRendererStructures();

    private OverlayRendererStructures()
    {
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        if (RendererToggle.OVERLAY_STRUCTURE_MAIN_TOGGLE.getBooleanValue() == false)
        {
            return false;
        }

        for (StructureType type : StructureType.VALUES)
        {
            if (type.isEnabled())
            {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        int hysteresis = 16;

        return DataStorage.getInstance().structureRendererNeedsUpdate() ||
               Math.abs(entity.getX() - this.lastUpdatePos.getX()) > hysteresis ||
               Math.abs(entity.getY() - this.lastUpdatePos.getY()) > hysteresis ||
               Math.abs(entity.getZ() - this.lastUpdatePos.getZ()) > hysteresis;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        int maxRange = (mc.options.viewDistance + 4) * 16;
        List<StructureData> data = this.getStructuresToRender(this.lastUpdatePos, maxRange);

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        if (data.isEmpty() == false)
        {
            this.renderStructureBoxes(data, cameraPos);
        }

        BUFFER_1.end();
        BUFFER_2.end();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);
    }

    private void renderStructureBoxes(List<StructureData> wrappedData, Vec3d cameraPos)
    {
        for (StructureData data : wrappedData)
        {
            StructureToggle toggle = data.getStructureType().getToggle();
            Color4f mainColor = toggle.getColorMain().getColor();
            Color4f componentColor = toggle.getColorComponents().getColor();
            this.renderStructure(data, mainColor, componentColor, cameraPos);
        }
    }

    private void renderStructure(StructureData structure, Color4f mainColor, Color4f componentColor, Vec3d cameraPos)
    {
        fi.dy.masa.malilib.render.RenderUtils.drawBox(structure.getBoundingBox(), cameraPos, mainColor, BUFFER_1, BUFFER_2);

        ImmutableList<IntBoundingBox> components = structure.getComponents();

        if (components.isEmpty() == false)
        {
            if (components.size() > 1 || MiscUtils.areBoxesEqual(components.get(0), structure.getBoundingBox()) == false)
            {
                for (IntBoundingBox bb : components)
                {
                    fi.dy.masa.malilib.render.RenderUtils.drawBox(bb, cameraPos, componentColor, BUFFER_1, BUFFER_2);
                }
            }
        }
    }

    private List<StructureData> getStructuresToRender(BlockPos playerPos, int maxRange)
    {
        ArrayListMultimap<StructureType, StructureData> structures = DataStorage.getInstance().getCopyOfStructureData();
        List<StructureData> data = new ArrayList<>();

        for (StructureType type : structures.keySet())
        {
            if (type.isEnabled() == false)
            {
                continue;
            }

            for (StructureData structure : structures.get(type))
            {
                if (MiscUtils.isStructureWithinRange(structure.getBoundingBox(), playerPos, maxRange))
                {
                    data.add(structure);
                }
            }
        }

        return data;
    }
}
