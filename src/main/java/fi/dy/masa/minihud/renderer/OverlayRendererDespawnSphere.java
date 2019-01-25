package fi.dy.masa.minihud.renderer;

import java.util.HashSet;
import javax.annotation.Nullable;
import org.lwjgl.opengl.GL11;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;

public class OverlayRendererDespawnSphere extends OverlayRendererBase
{
    private static final EnumFacing[] FACING_ALL = new EnumFacing[] { EnumFacing.DOWN, EnumFacing.UP, EnumFacing.NORTH, EnumFacing.SOUTH, EnumFacing.WEST, EnumFacing.EAST };
    private static final EnumFacing[] HORIZONTALS = new EnumFacing[] { EnumFacing.NORTH, EnumFacing.SOUTH, EnumFacing.WEST, EnumFacing.EAST };
    //private static final EnumFacing[] HORIZONTALS_ROTATING = new EnumFacing[] { EnumFacing.EAST, EnumFacing.NORTH, EnumFacing.WEST, EnumFacing.SOUTH };
    public static Vec3d newPos;

    protected Vec3d lastUpdatePos = Vec3d.ZERO;
    protected long lastUpdateTime;

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return RendererToggle.OVERLAY_DESPAWN_SPHERE.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        /*
        return System.currentTimeMillis() - this.lastUpdateTime >= 1000 &&
                (this.lastUpdatePos.x != entity.posX ||
                 this.lastUpdatePos.y != entity.posY ||
                 this.lastUpdatePos.z != entity.posZ);
        */
        return newPos != null;
    }

    @Override
    public void update(Entity entity, Minecraft mc)
    {
        Vec3d center = entity.getPositionVector();

        if (newPos != null)
        {
            center = newPos;
            newPos = null;
        }
        //center = new Vec3d(256.5, 10, 256.5); // FIXME debug

        if (Configs.Generic.DESPAWN_SPHERE_ROUND.getBooleanValue())
        {
            this.renderSphereRound(center, mc);
        }
        else
        {
            this.renderSphereBlock(center, mc);
        }

        this.lastUpdatePos = entity.getPositionVector();
        this.lastUpdateTime = System.currentTimeMillis();
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
        this.allocateBuffer(GL11.GL_TRIANGLES);
    }

    protected void renderSphereRound(Vec3d center, Minecraft mc)
    {
        RenderObjectBase renderTriangles = this.renderObjects.get(2);
        BUFFER_1.begin(renderTriangles.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        Color4f color = Configs.Colors.DESPAWN_SPHERE_OVERLAY_COLOR.getColor();

        renderSphere(center, 8, 4, BUFFER_1, color);

        BUFFER_1.finishDrawing();

        renderTriangles.uploadData(BUFFER_1);
    }

    protected void renderSphereBlock(Vec3d center, Minecraft mc)
    {
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        Color4f colorQuad = Configs.Colors.DESPAWN_SPHERE_OVERLAY_COLOR.getColor();
        Color4f colorLine = Color4f.fromColor(colorQuad, 1);
        BlockPos posCenter = new BlockPos(center);
        BlockPos.MutableBlockPos posMutable = new BlockPos.MutableBlockPos();
        //ArrayList<BlockPos> positionsOnRing = new ArrayList<>();
        HashSet<BlockPos> spherePositions = new HashSet<>();

        long before = System.nanoTime();
        posMutable.setPos(posCenter);
        addPositionsOnRing(spherePositions, posMutable, center);

        for (int i = 1; i < 130; ++i)
        {
            posMutable.setPos(posCenter.getX(), posCenter.getY() - i, posCenter.getZ());
            addPositionsOnRing(spherePositions, posMutable, center);

            posMutable.setPos(posCenter.getX(), posCenter.getY() + i, posCenter.getZ());
            addPositionsOnRing(spherePositions, posMutable, center);
        }
        System.out.printf("time: %.6f s\n", (double) (System.nanoTime() - before) / 1000000000D);

        int r = 0;
        Color4f colorDebug = new Color4f(0, 0.2f, 0.7f, 0.4f);

        for (BlockPos pos : spherePositions)
        {
            //fi.dy.masa.malilib.render.RenderUtils.drawBlockBoundingBoxSidesBatchedQuads(pos, colorDebug, -0.25, BUFFER_1);
            for (int i = 0; i < 6; ++i)
            {
                EnumFacing side = FACING_ALL[i];

                if (isAdjacentPositionOutside(pos, center, side))
                {
                    renderBlockSideQuads(pos, side, BUFFER_1, colorQuad);
                    renderBlockSideLines(pos, side, BUFFER_2, colorLine);
                    r++;
                }
            }
        }
        //System.out.printf("rendered: %d\n", r);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);
    }

    private static void addPositionsOnRing(HashSet<BlockPos> positions, BlockPos.MutableBlockPos posMutable, Vec3d center)
    {
        EnumFacing direction = EnumFacing.EAST;

        if (movePositionToRing(posMutable, center, direction))
        {
            BlockPos posFirst = posMutable.toImmutable();
            positions.add(posFirst);
            int failsafe = 860;

            while (--failsafe > 0)
            {
                direction = getNextPositionOnRing(posMutable, center, direction);

                if (direction == null || posMutable.equals(posFirst))
                {
                    break;
                }

                positions.add(posMutable.toImmutable());
            }

            //System.out.printf("y: %d - positions: %d\n", posMutable.getY(), positions.size());
        }
    }

    private static boolean movePositionToRing(BlockPos.MutableBlockPos posMutable, Vec3d center, EnumFacing dir)
    {
        final double maxDistSq = 128 * 128;
        int x = posMutable.getX();
        int y = posMutable.getY();
        int z = posMutable.getZ();
        int xNext = x;
        int yNext = y;
        int zNext = z;
        int failsafe = 0;
        int failsafeMax = 140;

        while (center.squareDistanceTo(xNext + 0.5, y, zNext + 0.5) < maxDistSq && ++failsafe < failsafeMax)
        {
            x = xNext;
            y = yNext;
            z = zNext;
            xNext += dir.getXOffset();
            yNext += dir.getYOffset();
            zNext += dir.getZOffset();
        }

        // Successfully entered the loop at least once
        if (failsafe > 0)
        {
            posMutable.setPos(x, y, z);
            return true;
        }

        return false;
    }

    @Nullable
    private static EnumFacing getNextPositionOnRing(BlockPos.MutableBlockPos posMutable, Vec3d center, EnumFacing dir)
    {
        EnumFacing dirOut = dir;
        EnumFacing ccw90 = getNextDirRotating(dir);
        final double maxDistSq = 128 * 128;
        final int y = posMutable.getY();

        for (int i = 0; i < 4; ++i)
        {
            int x = posMutable.getX() + dir.getXOffset();
            int z = posMutable.getZ() + dir.getZOffset();

            // First check the adjacent position
            double dist = center.squareDistanceTo(x + 0.5, y, z + 0.5);
            //if (posMutable.getZ() <= 168 && posMutable.getX() >= 343) System.out.printf("1 pos: x: %d, z: %d - check: x: %d, z: %d, dist: %.3f, out: %s\n", posMutable.getX(), posMutable.getZ(), x, z, dist, dirOut);

            if (dist < maxDistSq)
            {
                posMutable.setPos(x, y, z);
                return dirOut;
            }

            // Then check the diagonal position
            x = posMutable.getX() + dir.getXOffset() + ccw90.getXOffset();
            z = posMutable.getZ() + dir.getZOffset() + ccw90.getZOffset();
            dist = center.squareDistanceTo(x + 0.5, y, z + 0.5);
            //if (posMutable.getZ() <= 168 && posMutable.getX() >= 343) System.out.printf("2 pos: x: %d, z: %d - check: x: %d, z: %d, dist: %.3f, out: %s\n", posMutable.getX(), posMutable.getZ(), x, z, dist, dirOut);

            if (dist < maxDistSq)
            {
                posMutable.setPos(x, y, z);
                return dirOut;
            }

            // Delay the next direction by one cycle, so that it won't get updated too soon on the diagonals
            dirOut = dir;
            dir = getNextDirRotating(dir);
            ccw90 = getNextDirRotating(dir);
        }

        //System.out.printf("3 null @ x: %d, z: %d\n", posMutable.getX(), posMutable.getZ());
        return null;
    }

    private static boolean isAdjacentPositionOutside(BlockPos pos, Vec3d center, EnumFacing dir)
    {
        final double maxDistSq = 128 * 128;
        return center.squareDistanceTo(pos.getX() + dir.getXOffset() + 0.5, pos.getY() + dir.getYOffset(), pos.getZ() + dir.getZOffset() + 0.5) >= maxDistSq;
    }

    /*
    private static boolean isPositionOnRing(int x, int y, int z, Vec3d center)
    {
        final double maxDistSq = 128 * 128;

        if (center.squareDistanceTo(x + 0.5, y, z + 0.5) < maxDistSq)
        {
            for (int i = 0; i < 4; ++i)
            {
                EnumFacing dir = HORIZONTALS[i];
                double dist = center.squareDistanceTo(x + dir.getXOffset() + 0.5, y, z + dir.getZOffset() + 0.5);

                // The given position has at least one outside neighbor, while being within the distance itself
                if (dist > maxDistSq)
                {
                    return true;
                }
            }
        }

        return false;
    }
    */

    /**
     * Returns the next horizontal direction in the EnumFacing sequence
     * @param firstDir
     * @return
     */
    /*
    private static EnumFacing getNextDir(EnumFacing firstDir)
    {
        int index = firstDir.getIndex();

        if (++index >= 6 || index < 2)
        {
            index = 2;
        }

        return HORIZONTALS[index - 2];
    }
    */

    /**
     * Returns the next horizontal direction in sequence, rotating counter-clockwise
     * @param firstDir
     * @return
     */
    private static EnumFacing getNextDirRotating(EnumFacing dirIn)
    {
        switch (dirIn)
        {
            case EAST:  return EnumFacing.NORTH;
            case NORTH: return EnumFacing.WEST;
            case WEST:  return EnumFacing.SOUTH;
            case SOUTH: return EnumFacing.EAST;
            default:    return EnumFacing.NORTH;
        }
    }

    public static void renderSphere(Vec3d center, final double radius, final int numRings, BufferBuilder buffer, Color4f color)
    {
        //final double ringHeight = (Math.PI * radius) / numRings;
        final double ringAngle = Math.PI / numRings;
        double[] x = new double[2 * numRings + 1];
        double[] z = new double[2 * numRings + 1];
        double yOffPrev = 0;
        System.out.printf("rendering...\n");

        for (int ring = 0; ring < numRings / 2; ++ring)
        {
            double angleRingTop = ( ring      * ringAngle);
            double angleRingBot = ((ring + 1) * ringAngle);

            double yOff2 = radius * Math.cos(angleRingBot);
            double y1 = center.y + radius - yOffPrev;
            double y2 = center.y + radius - yOff2;

            double ry1 = radius * Math.sin(angleRingTop);
            double ry2 = radius * Math.sin(angleRingBot);

            if (ring == 0)
            {
                System.out.printf("ringAngle: %.2f, angleRingTop: %.2f, angleRingBot: %.2f, yOff2: %.2f, y1: %.2f, y2: %.2f, ry1: %.2f, ry2: %.2f\n", ringAngle, angleRingTop, angleRingBot, yOff2, y1, y2, ry1, ry2);
            }

            if (ring == 0)
            {
                for (int tr = 0; tr < (2 * numRings); ++tr)
                {
                    if (tr == 0)
                    {
                        double a1 = (tr * ringAngle);
                        double x2 = ry2 * Math.sin(a1) + center.x;
                        double z2 = ry2 * Math.cos(a1) + center.z;
                        x[tr] = x2;
                        z[tr] = z2;
                    }

                    double a2 = ((tr + 1) * ringAngle);
                    double x3 = ry2 * Math.sin(a2) + center.x;
                    double z3 = ry2 * Math.cos(a2) + center.z;

                    x[tr + 1] = x3;
                    z[tr + 1] = z3;

                    //System.out.printf("2: x: %.2f, y: %.2f, z: %.2f\n", x[tr    ], y2, z[tr    ]);
                    //System.out.printf("3: x: %.2f, y: %.2f, z: %.2f\n", x[tr + 1], y2, z[tr + 1]);

                    // Top of the sphere
                    buffer.pos(center.x , y1, center.z ).color(color.r, color.g, color.b, color.a).endVertex();
                    buffer.pos(x[tr    ], y2, z[tr    ]).color(color.r, color.g, color.b, color.a).endVertex();
                    buffer.pos(x[tr + 1], y2, z[tr + 1]).color(color.r, color.g, color.b, color.a).endVertex();

                    // Mirrored to the bottom of the sphere
                    double y1b = center.y - radius + yOffPrev;
                    double y2b = center.y - radius + yOff2;
                    buffer.pos(center.x , y1b, center.z ).color(color.r, color.g, color.b, color.a).endVertex();
                    buffer.pos(x[tr    ], y2b, z[tr    ]).color(color.r, color.g, color.b, color.a).endVertex();
                    buffer.pos(x[tr + 1], y2b, z[tr + 1]).color(color.r, color.g, color.b, color.a).endVertex();
                }
            }
            else
            {
                /*
                for (int tr = 0; tr < numRings; ++tr)
                {
                    double a2 = Math.PI - ((tr + 1) * ringAngle);
                    double x3 = ry2 * Math.sin(a2) + ry2 * Math.cos(a2);
                    double z3 = ry2 * Math.cos(a2) + ry2 * Math.sin(a2);

                    if (tr == 0)
                    {
                        double a1 = Math.PI - (tr * ringAngle);
                        double x2 = ry2 * Math.sin(a1) + ry2 * Math.cos(a1);
                        double z2 = ry2 * Math.cos(a1) + ry2 * Math.sin(a1);
                        x[tr] = x2;
                        z[tr] = z2;
                    }

                    x[tr + 1] = x3;
                    z[tr + 1] = z3;

                    buffer.pos(center.x , y1, center.z ).color(color.r, color.g, color.b, color.a).endVertex();
                    buffer.pos(x[tr    ], y2, x[tr    ]).color(color.r, color.g, color.b, color.a).endVertex();
                    buffer.pos(x[tr + 1], y2, x[tr + 1]).color(color.r, color.g, color.b, color.a).endVertex();
                }
                */
            }

            yOffPrev = yOff2;
        }
    }

    public static void renderBlockSideQuads(BlockPos pos, EnumFacing side, BufferBuilder buffer, Color4f color)
    {
        double x = pos.getX();
        double y = pos.getY();
        double z = pos.getZ();

        switch (side)
        {
            case DOWN:
                buffer.pos(x    , y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case UP:
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case NORTH:
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case SOUTH:
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case WEST:
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case EAST:
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                break;
        }
    }

    public static void renderBlockSideLines(BlockPos pos, EnumFacing side, BufferBuilder buffer, Color4f color)
    {
        double x = pos.getX();
        double y = pos.getY();
        double z = pos.getZ();

        switch (side)
        {
            case DOWN:
                buffer.pos(x    , y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case UP:
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case NORTH:
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case SOUTH:
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case WEST:
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
            case EAST:
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y + 1, z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z + 1).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y    , z    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;
        }
    }
}
