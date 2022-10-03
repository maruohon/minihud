package fi.dy.masa.minihud.renderer;

import java.util.Collection;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.render.ShapeRenderUtils;
import fi.dy.masa.malilib.render.overlay.BaseRenderObject;
import fi.dy.masa.malilib.util.data.Color4f;
import fi.dy.masa.malilib.util.game.wrap.EntityWrap;
import fi.dy.masa.malilib.util.game.wrap.GameUtils;
import fi.dy.masa.malilib.util.position.IntBoundingBox;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.structure.StructureData;
import fi.dy.masa.minihud.data.structure.StructureStorage;
import fi.dy.masa.minihud.data.structure.StructureType;
import fi.dy.masa.minihud.util.MiscUtils;

public class OverlayRendererStructures extends MiniHUDOverlayRenderer
{
    @Override
    public boolean shouldRender(Minecraft mc)
    {
        // TODO use a cached value for any types enabled?
        return RendererToggle.STRUCTURE_BOUNDING_BOXES.isRendererEnabled();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        int hysteresis = 16;

        return StructureStorage.INSTANCE.hasStructureDataChanged() ||
               Math.abs(EntityWrap.getX(entity) - this.lastUpdatePos.getX()) > hysteresis ||
               Math.abs(EntityWrap.getY(entity) - this.lastUpdatePos.getY()) > hysteresis ||
               Math.abs(EntityWrap.getZ(entity) - this.lastUpdatePos.getZ()) > hysteresis;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, Minecraft mc)
    {
        BaseRenderObject renderQuads = this.renderObjects.get(0);
        BaseRenderObject renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        this.updateStructures(this.lastUpdatePos, cameraPos);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);
    }

    private void updateStructures(BlockPos playerPos, Vec3d cameraPos)
    {
        ArrayListMultimap<StructureType, StructureData> structures = StructureStorage.INSTANCE.getStructureDataAndClearDirtyFlag();
        int maxRange = (GameUtils.getRenderDistanceChunks() + 2) * 16;

        for (StructureType type : structures.keySet())
        {
            if (type.isEnabled())
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
            if (MiscUtils.isStructureWithinRange(structure.getEnclosingBox(), playerPos, maxRange))
            {
                this.renderStructure(type, structure, cameraPos);
            }
        }
    }

    private void renderStructure(StructureType type, StructureData structure, Vec3d cameraPos)
    {
        Color4f color = type.getToggle().getColorMain().getColor();
        ImmutableList<IntBoundingBox> components = structure.getComponents();

        ShapeRenderUtils.renderBoxSidesAndEdges(structure.getEnclosingBox(), color, BUFFER_1, BUFFER_2, cameraPos);
        int size = components.size();

        // Don't render the component box if there is only one component
        // and the box is the same as the enclosing box (for example a proper, "non-broken" Swamp Hut)
        if (size >= 1 &&
            (size > 1 || components.get(0).equals(structure.getEnclosingBox()) == false))
        {
            color = type.getToggle().getColorComponents().getColor();

            for (IntBoundingBox bb : components)
            {
                ShapeRenderUtils.renderBoxSidesAndEdges(bb, color, BUFFER_1, BUFFER_2, cameraPos);
            }
        }
    }
}
