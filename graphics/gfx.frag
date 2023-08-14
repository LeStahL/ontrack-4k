// ontrack-4k
// Copyright (C) 2023  Alexander Kraus <nr4@z10.info>

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#version 450

out vec4 out_color;

layout(location = 0) uniform int iFrame;
layout(location = 1) uniform int iSample;
layout(location = 2) uniform int iPass;
layout(location = 3) uniform ivec2 iResolutionInteger;
layout(location = 4) uniform sampler2D iChannel0;

float iTime = float(iSample) / 44100.;
vec2 iResolution = vec2(iResolutionInteger);

const vec3 c = vec3(1,0,-1);
const float pi = 3.14159;
float tj,
    dt,
    bpm = 65.,
    spb,
    nbeats,
    scale,
    stepTime,
    hardBeats,
    expandRings,
    sineExpandRings,
    ringHeight,
    colorEscalation,
    glowEscalation,
    colorCandyNess,
    flashBackground;

// Created by David Hoskins and licensed under MIT.
// See https://www.shadertoy.com/view/4djSRW.
// float->float hash function
float hash11(float p)
{
    p = fract(p * .1031);
    p *= p + 33.33;
    p *= p + p;
    return fract(p);
}

// Created by David Hoskins and licensed under MIT.
// See https://www.shadertoy.com/view/4djSRW.
// vec2->float hash function
float hash12(vec2 p)
{
	vec3 p3  = fract(vec3(p.xyx) * .1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

const mat3 Msrgb = mat3(
    0.4124564, 0.2126729, 0.0193339,
    0.3575761, 0.7151522, 0.1191920,
    0.1804375, 0.0721750, 0.9503041
), M1 = mat3(
    0.8189330101, 0.0329845436, 0.0482003018,
    0.3618667424, 0.9293118715, 0.2643662691,
    -0.1288597137, 0.0361456387, 0.6338517070
), M2 = mat3(
    0.2104542553, 1.9779984951, 0.0259040371,
    0.7936177850, -2.4285922050, 0.7827717662,
    -0.0040720468, 0.4505937099, -0.8086757660
);

// Convert rgb to xyz (sRGB) - compare http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
vec3 rgb2xyz_srgb(vec3 rgb) {
    return Msrgb * rgb;
}

// Convert xyz to rgb (sRGB) - compare http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
vec3 xyz2rgb_srgb(vec3 xyz) {
    return inverse(Msrgb) * xyz;
}

// Convert xyz to oklab - compare https://bottosson.github.io/posts/oklab/
vec3 xyz2oklab(vec3 xyz) {
    return M2 * pow(M1 * xyz, c.xxx/3.);
}

// Convert oklab to xyz - compare https://bottosson.github.io/posts/oklab/
vec3 oklab2xyz(vec3 lab) {
    return inverse(M1) * pow(inverse(M2) * lab, 3.*c.xxx);
}

// Convert oklab to oklch - compare https://bottosson.github.io/posts/oklab/
vec3 oklab2oklch(vec3 lab) {
    return vec3(lab.x, length(lab.yz), atan(lab.z, lab.y));
}

// Convert oklch to oklab - compare https://bottosson.github.io/posts/oklab/
vec3 oklch2oklab(vec3 lch) {
    return vec3(lch.x, lch.y * vec2(cos(lch.z), sin(lch.z)));
}

const int COLOR_COUNT = 8;
vec3 gradientColors[] = vec3[](
        c.yyy,
        vec3(0.02,0.16,0.24),
        vec3(0.03,0.32,0.44),
        vec3(0.47,0.68,0.73),
        vec3(0.67,0.16,0.12),
        vec3(0.96,0.52,0.00),
        vec3(0.99,0.78,0.31),
        vec3(1.00,0.92,0.80)
);

vec3 weightedOklabLinearGradientOklab(float amount) {
    amount = fract(amount);
    // First rescale amount to match the distance in the color space.
    float colorspaceDistances[COLOR_COUNT],
        steps[COLOR_COUNT];
    float totalColorspaceDistance = 0.;
    for(int i=0; i<COLOR_COUNT; ++i) {
        vec3 c1 = (xyz2oklab(rgb2xyz_srgb(gradientColors[(i+1) % COLOR_COUNT]))),
            c2 = (xyz2oklab(rgb2xyz_srgb(gradientColors[i])));
        //colorspaceDistances[i] = abs(c1.x-c2.x) + length(c1.yz-c2.yz);
        colorspaceDistances[i] = length(c1 - c2);
        totalColorspaceDistance += colorspaceDistances[i];
    }
    
    // Normalize weights
    float currentStep = 0.;
    for(int i=0; i<COLOR_COUNT; ++i) {
        colorspaceDistances[i] /= totalColorspaceDistance;
        steps[i] = currentStep;
        currentStep += colorspaceDistances[i];
    }
    
    // Determine color mixing
    for(int i=0; i<COLOR_COUNT; ++i) {
        if(amount < steps[(i + 1) % COLOR_COUNT]) {
            return xyz2rgb_srgb(oklab2xyz((
                mix(
                    (xyz2oklab(rgb2xyz_srgb(gradientColors[i % COLOR_COUNT]))),
                    (xyz2oklab(rgb2xyz_srgb(gradientColors[(i+1) % COLOR_COUNT]))),
                    (amount-steps[i % COLOR_COUNT])/(steps[(i+1) % COLOR_COUNT] - steps[i % COLOR_COUNT])
                )
            )));
        }
    }
    
    return xyz2rgb_srgb(oklab2xyz((
        mix(
            (xyz2oklab(rgb2xyz_srgb(gradientColors[COLOR_COUNT - 1]))),
            (xyz2oklab(rgb2xyz_srgb(gradientColors[0]))),
            abs(amount-steps[COLOR_COUNT - 1])/abs(1.-steps[COLOR_COUNT - 1])
        )
    )));
}

vec3 lpos1, 
    lpos2, 
    lpos3, 
    scatter;

void dmin(inout vec4 d, float x, float y, float z, float w)
{
	if( x < d.x ) d = vec4(x, y, z, w);
}

// Method by fizzer
vec3 hashHs(vec3 n, float seed)
{
    float u = hash11( 78.233 + seed),
        v = hash11( 10.873 + seed),
        a = 6.2831853 * v;
    u = 2.0*u - 1.0;
    return normalize( n + vec3(sqrt(1.0-u*u) * vec2(cos(a), sin(a)), u) );
}

// From https://iquilezles.org/articles/distfunctions; written by iq.
float sdTorus(vec3 p, vec2 t)
{
  vec2 q = vec2(length(p.xz)-t.x,p.y);
  return length(q)-t.y;
}

float lfnoise(vec2 t)
{
    vec2 i = floor(t);
    t = fract(t);
    t = smoothstep(c.yy, c.xx, t);
    vec2 v1 = vec2(hash12(i), hash12(i+c.xy)), 
        v2 = vec2(hash12(i+c.yx), hash12(i+c.xx));
    v1 = c.zz+2.*mix(v1, v2, t.y);
    return mix(v1.x, v1.y, t.x);
}

float e = .05 * iTime;
mat2 MM = mat2(cos(e), -sin(e), sin(e), cos(e));

vec4 map(vec3 p,bool flag)
{
    p.xy *= MM;

    vec4 d = vec4(1),
        q;
    
    // point lights
    float re = min(length(p-lpos1), length(p-lpos2)) - .003;
    dmin(d, re, 1.,0.,.8);
    if(flag) scatter += max(-(re-.7),0.) * .49 * weightedOklabLinearGradientOklab(.6);

    // Outer Silo
    float pxs = 1.2;
    float x = mod(p.x, pxs)-.5*pxs,
        xj = p.x - x;
    float db = sdTorus(vec3(p.y, x, p.z), vec2(6., .5*pxs));
    dmin(d, db, 0., 0., 0.);
    if(flag) scatter += max(-(db-1.1),0.) * .26 * flashBackground * .5 * c.xxx * step(.5, hash12(vec2(xj, tj) + .5));

    p.y += (.4 + .4 * step(29., iTime) * step(iTime, 59.) ) * iTime;
        
    // Silo
    float ps = (.1 + .9 * ringHeight) * (.05 + .95 * hash11(tj + .1));
    float y = mod(p.y, ps)-.5*ps,
        yj = p.y - y+13.3;
        
    float da = sdTorus(vec3(p.x, y, p.z), vec2(1. + expandRings * hash12(vec2(yj, tj) + .2) + 2. * sineExpandRings * lfnoise(vec2(yj, tj) + .2), .5*ps));
    da = min(da, sdTorus(vec3(p.x, y-ps, p.z), vec2(1. + expandRings * hash12(vec2(yj + ps, tj) + .2)  + 2. * sineExpandRings * lfnoise(vec2(yj + ps, tj) + .2), .5*ps)));
    da = min(da, sdTorus(vec3(p.x, y+ps, p.z), vec2(1. + expandRings * hash12(vec2(yj - ps, tj) + .2)  + 2. * sineExpandRings * lfnoise(vec2(yj - ps, tj) + .2), .5*ps)));
    float c1 = colorEscalation * step(hash12(vec2(yj, tj) + .1),.2 + .8 * colorCandyNess) * hash12(vec2(yj, tj) + .4);
    dmin(d, da, 0., 0., c1);
    if(flag) scatter += max(-(da-1.1),0.) * .26 * glowEscalation * .5 * mix(weightedOklabLinearGradientOklab(c1), c.yyy, 1.-scale) * scale;
    
    // Center of silo
    float dc = length(p.xz) - .03;
    dmin(d, dc, 1., 0., .8);
    if(flag) scatter += max(-(dc-.7),0.) * .49 * weightedOklabLinearGradientOklab(.6);
    
	return d;
}

vec3 normal(vec3 p)
{
	vec2 e = vec2(0, .001);
	return normalize(map(p,false).x-vec3(map(p - e.yxx,false).x, map(p - e.xyx,false).x, map(p - e.xxy,false).x));
}

// From gltracy https://www.shadertoy.com/view/lsXSz7
vec3 radiance(
    vec3 n,		// macro surface normal
    vec3 l,		// direction from vertex to light
    vec3 v,		// direction from vertex to view
    // matt
    float m	// roughness
) {
    vec3 cdiff = vec3(.8);
    vec3 cspec = vec3(.7);
    vec3 clight = vec3(.7, .65, .8);

    // half vector
    vec3 h = normalize( l + v );

    // dot
    float dot_n_h = max( dot( n, h ), .01 );
    float dot_n_v = max( dot( n, v ), .01 );
    float dot_n_l = max( dot( n, l ), .1 );
    float dot_h_v = max( dot( h, v ), .01 ); // dot_h_v == dot_h_l

    // Geometric Term
    // Cook-Torrance
    //          2 * ( N dot H )( N dot L )    2 * ( N dot H )( N dot V )
    // min( 1, ----------------------------, ---------------------------- )
    //                 ( H dot V )                   ( H dot V )
    float g = 2. * dot_n_h / dot_h_v;
    float G = min( min( dot_n_v, dot_n_l ) * g, 1. );

    // Normal Distribution Function ( cancel 1 / pi )
     // Beckmann distribution
    //         ( N dot H )^2 - 1
    //  exp( ----------------------- )
    //         ( N dot H )^2 * m^2
    // --------------------------------
    //         ( N dot H )^4 * m^2
    float sq_nh   = dot_n_h * dot_n_h;
    float sq_nh_m = sq_nh * ( m * m );
    float D = exp( ( sq_nh - 1. ) / sq_nh_m ) / ( sq_nh * sq_nh_m );

    // Specular Fresnel Term : Schlick approximation
    // F0 + ( 1 - F0 ) * ( 1 - ( H dot V ) )^5
    vec3 Fspec = cspec + ( 1.  - cspec ) * pow( 1. - dot_h_v, 5. );

    // Diffuse Fresnel Term : violates reciprocity...
    // F0 + ( 1 - F0 ) * ( 1 - ( N dot L ) )^5
    vec3 Fdiff = cspec + ( 1.  - cspec ) * pow( 1. - dot_n_l, 5. );

    // Cook-Torrance BRDF
    //          D * F * G
    // ---------------------------
    //  4 * ( N dot V )( N dot L )
    vec3 brdf_spec = Fspec * D * G / ( dot_n_v * dot_n_l * 4. );

    // Lambertian BRDF ( cancel 1 / pi )
    vec3 brdf_diff = cdiff * ( 1. - Fdiff );

    // Punctual Light Source ( cancel pi )
    return ( brdf_spec + brdf_diff ) * clight * dot_n_l;
}

// The MADtracer, originally written by Virgill/Alcatraz.
// https://www.shadertoy.com/view/ttlXRf
// I made some modifications to include cook-torrance illumination. 
void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    lpos1 = vec3(0); 
    lpos2=vec3(0); 
    lpos3=vec3(0,1,0); 
    scatter =vec3(0);

    spb = 60. / bpm;
    dt = mod(iTime, spb);
    tj = iTime - dt;
    bool a = int(round(tj/spb)) % 4 == 3;
    
    if(a) {
        iTime -= .375 * spb;
        dt = mod(iTime, spb);
        tj = iTime - dt;
    }

    stepTime = mod(iTime+.5*spb, spb)-.5*spb;
    nbeats = tj / spb;
    scale = smoothstep(0., .05 * spb, stepTime)*smoothstep(.7*spb, 0., stepTime);
    hardBeats = round((iTime-mod(iTime, spb))/spb);

    if(iTime > 73.) {
        tj = mod(73., spb);
        scale = 0.;
        nbeats = tj / spb;
    }

    if(a) iTime += .375 * spb;

    float sa = 6.;
    colorEscalation = smoothstep(sa, 1.5*sa, nbeats) * (1. - smoothstep(12. *sa, 12.5*sa, nbeats));
    glowEscalation = smoothstep(2.*sa, 2.5*sa, nbeats);
    colorCandyNess = smoothstep(3.*sa, 3.5*sa, nbeats);
    ringHeight = smoothstep(4.*sa, 4.5*sa, nbeats) * (1. - smoothstep(10.*sa, 10.5*sa, nbeats));
    sineExpandRings = smoothstep(5.*sa, 5.5*sa, nbeats) * (1. - smoothstep(9.*sa, 9.5*sa, nbeats));
    expandRings = smoothstep(6.*sa, 6.5*sa, nbeats) * (1. - smoothstep(8.*sa, 8.5*sa, nbeats));
    flashBackground = smoothstep(7.*sa, 7.5*sa, nbeats);
    
    fragColor = c.yyyx;
	vec2 uv = fragCoord.xy / iResolution.xy;

	// borders :(
	if(uv.y>.11 && uv.y<.89)
	{
        // camera
		vec3 ro1 = vec3(0, 0, -5.), rd1 = normalize(vec3((2.*fragCoord.xy-iResolution.xy)/iResolution.x, 1));
		float t1 = 0., t2 = 0.,t3=0., seed = 0.;

//***************************************************************************************************
// Cast ray
//***************************************************************************************************     
  
        lpos1 = - 4.*c.yyx+.3*vec3(1.*cos(iTime), .3*sin(iTime), .5+sin(.5*iTime));				// position point light 1 
        lpos2 = - 4.*c.yyx+.3*vec3(1.2*cos(.3*iTime), .4*sin(.4*iTime), .5+sin(.2*iTime));   	// position point light 2
       
        seed=hash12(uv + 13.)+fract(float(iFrame)*1.e-3) + 137.;

		vec3 scol=vec3(0);
        vec4 m1, m2, m3;
        vec3 ro2,rd2,nor1, nor2,pos1,pos2,pos3;
        
        float roughness = .2;
	    
		for( int i = 0; i < 250; i++ )
		{
            // every iteration a new seed
            seed=32.+seed*fract(seed);
           	
            // raymarch 1st pass
           	pos1 = ro1+rd1*t1;		
        	m1 = map(pos1,true);
			t1+=0.2*(m1.x); 	// the smaller the factor, the thicker the media
			
            if (m1.x<.001)		// hit
            {
                nor1 = normal(pos1); 
        
                // cook torrance
                scol += 5.e1 * weightedOklabLinearGradientOklab(m1.w) * (.3 + .1 * radiance(nor1, normalize(lpos1-pos1), normalize(ro1-pos1), roughness));
                scol += 5.e1 * weightedOklabLinearGradientOklab(m1.w) * (.3 + .1 * radiance(nor1, normalize(lpos2-pos1), normalize(ro1-pos1), roughness));

              	break;
            }

          
			// Raymarch direct light 
            lpos3 = vec3(0.,1.,0.);
            lpos3 = mix(lpos3,hashHs(lpos3,seed),.15); 	// add randomness
        	pos2=pos1+lpos3*t2;							// calculate ray direction
            m2 = map(pos2,false);
            t2+=m2.x;
        }

	  	nor1 = normal(pos1); 

        // bounce        
        m1.y=clamp(m1.y,0.,1.);
		t2=0.;
       	for( int i = 1; i < 100 ; i++ )
      	{
            // every iteration a new seed
            seed=32.+seed*fract(seed);            
			rd2 = mix(reflect(rd1,nor1),hashHs(nor1,seed),m1.y);	// reflect depending on material	
            pos2 = pos1+ rd2*t2;
			m2 = map(pos2,false); 
            t2+=.2*m2.x;
            scol += .4 * c.xxx*step(1., m2.y);
        }
        
        if (m2.x<.001) {
            nor2 = normal(pos2);
            
            scol += 5.e1 * weightedOklabLinearGradientOklab(m2.w) * (.3 + .1* radiance(nor2, normalize(lpos1-pos2), normalize(ro1-pos2), .5* roughness));
            scol += 5.e1 * weightedOklabLinearGradientOklab(m2.w) * (.3 + .1* radiance(nor2, normalize(lpos2-pos2), normalize(ro1-pos2), .5* roughness));
        }

		fragColor = vec4((.003*scol+0.02*scatter+1.*texture(iChannel0, uv).xyz), 0.)*.8; // with blur
        fragColor = clamp(fragColor,0.,1.);
	}
}

void main() {
    if(iPass == 0) {
        // mainImage(out_color, gl_FragCoord.xy);
        float ssaa = 9.;
        out_color = vec4(0.);
        float bound = sqrt(ssaa)-1.;
            for(float i = -.5*bound; i<=.5*bound; i+=1.)
                for(float j=-.5*bound; j<=.5*bound; j+=1.)
                {
                    vec4 c1;
                    float r = pi/4.;
                    mat2 R = mat2(cos(r),sin(r),-sin(r),cos(r));
                    mainImage(c1, gl_FragCoord.xy+R*(vec2(i,j)*1./max(bound, 1.)));
                    out_color += c1;
                }
        out_color /= ssaa;
    }
    else out_color = texture(iChannel0, gl_FragCoord.xy/iResolution.xy);

    out_color.rgb *= smoothstep(0., 2., iTime) * smoothstep(76., 74., iTime);
}
