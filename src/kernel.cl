__kernel void duparray(__global float *in, __global float *out )
{
	int id = get_global_id(0);
	out[id] = 2*in[id];
}
