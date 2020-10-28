#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cassert>

constexpr double WALPHA = 0.000001;
const double WLAMBDA = 0.0001;
constexpr bool WSIGMOID = true;
constexpr double WL = 0;
constexpr bool WLEARN = true;
int Wcount = 0;
int64_t WerrCount = 0;
constexpr double Ppos = 0.01367483420370993463713956354413;
constexpr double PosTH = 0.5;

constexpr int WBatchsize = 10000;
constexpr bool WPrintData = false;
constexpr bool USE_BATCH_ERROR = true;

int Winit = false;
double Werr = 0;
std::vector<double> W, Wdelta;
std::vector<double> Wstart;
//-----------------
//= {-0.79235, -0.198703, -0.218553, -0.375093, -0.0504252, -0.741924, -0.170643, -0.0731435, -0.104914, -0.212673, -0.0798104, -0.151165};
//= {-0.68274, -0.169885, -0.187122, -0.325733, -0.040028, -0.642712, -0.148139, -0.06269, -0.09036, -0.185674, -0.0683509, -0.127526};
//= {-0.493165, -0.121028, -0.133636, -0.238501, -0.025466, -0.467699, -0.108182, -0.0448424, -0.0651283, -0.136877, -0.0488468, -0.0892888};

void printW(std::ostream &out = std::cerr)
{
  out << "Err=" << Werr/WerrCount << " W:";
  for(int i = 0; i < (int)W.size(); ++i)
       out << ", " << W[i];
  out << std::endl;
}

void printWData(int T, const std::vector<int> &CV, std::ostream &out = std::cerr)
{
	out << T;
	for(int i = 0; i < (int)CV.size(); ++i)
		out << "," << CV[i];

        out << std::endl;
}

void learn(std::vector<int>& labels, std::vector<std::vector<int>> &attrs, std::ostream& out = std::cout)
{
      int N = (int)labels.size();
      int K = (int)attrs[0].size();
      assert(N == (int)attrs.size());
	  int classCount[2] = {0,0};
     
      

      std::vector<int> index(N);
      for(int i = 0; i < N; ++i)
	  {
            index[i] = i;
			++classCount[labels[i]];
	  }
	  
	  out << "N=" << N << " K=" << K << " pos=" << classCount[1] << " neg=" << classCount[0] 
	  << " batchSize=" << WBatchsize 
	  << " alpha=" << WALPHA << std::endl;
      //std::exit(0);
      
      
      if(!Winit)
      {
          W = Wstart;
          Winit = true;
          Wdelta.resize(K, 0);
          W.resize(K, 0);        
          out << "INIT" << std::endl;
          printW(out);
      }
	  
	  for(int epoch = 1;;++epoch)
	  {
		  out << "Start epoch "<< epoch << std::endl;
		  
		  std::random_shuffle(index.begin(), index.end());
		  Werr = 0;
		  WerrCount = 0;
		  
		  for(int batch = 0; batch < N; batch += WBatchsize)
		  {
				for(int k = 0; k < K; ++k)
					Wdelta[k] = 0;
				
				int count = std::min(batch + WBatchsize, N) - batch;
				
				if(USE_BATCH_ERROR)
				{
					Werr = 0;
					WerrCount = 0;
				}
				
				for(int j = batch; j < batch + count; ++j)
				{
					int ii = index[j];
					assert(K == (int)attrs[ii].size());
					
					double sum = 0;
					for(int i = 0; i < K; ++i)
						sum += W[i] * attrs[ii][i];

					double grad = 1;
					int target = (labels[ii] ? 1 : -1);
					bool predict = sum > PosTH;
					   if(WSIGMOID)
					   {
						  sum = 1 / (1 + std::exp(-sum));
						  target = (labels[ii] ? 1 : 0);
						  grad = sum * (1 - sum);
						  predict = sum > PosTH;
					   }
			   
					 double sumL = 0;
					 for(int i = 0; i < K; ++i)
						sumL += W[i] * W[i];
					
					 double err = sum - target;
					  //std::cerr << "=> T=" << T << std::endl;		  
					  //std::cerr << "=> sum=" << sum << std::endl;		  
					  //std::cerr << "=> err=" << err << std::endl;		  
					  
					  double loss = err*err + WL/K * sumL;
					  
					  		double w1 = 0.5 / (labels[ii] ? Ppos : 1 - Ppos);
							double w2 = (predict ? 1/Ppos : 0);
							double w = w1 * 1.00 + w2 * 0.00;
							
					  /*
					  if(Werr < 0)
						Werr = loss;
					  else
						Werr = (1-WLAMBDA)*Werr + WLAMBDA*loss;
					*/
					Werr += w * loss;
					WerrCount++;
					
				  
					  //std::cerr << "=> w=" << w << std::endl;		  
					  if(WPrintData)
					  {
						  printWData(labels[ii], attrs[ii]);
					  }
					  else if (WLEARN)
					  {
							for(int i = 0; i < K; ++i)
							{
								double delta = w * (  err * grad * attrs[ii][i] + 2*WL/K * W[i]);
								  //	std::cerr << "===> C=" << CV[i] << " delta=" << delta << std::endl;
								Wdelta[i] += delta;
							}
					  }
				}
				
				// batch learn
				if (WLEARN)
				{                                                 
					for(int i = 0; i < K; ++i)
					{                                  
						W[i] -= WALPHA * Wdelta[i];
					}
					printW(out);		
				}                            
		  }
	  }
	     
}

std::vector<int> labels;
std::vector<std::vector<int>> attrs;

void readData(std::vector<int>& labels, std::vector<std::vector<int>> &attrs, std::istream& in = std::cin)
{
    std::string line;
    while(std::getline(in, line))
    {
        std::stringstream str(line);
        int x;
        if(str >> x)
        {
            labels.push_back(x);
            std::vector<int> v;
	    char ch;
            while(str >> ch >> x)
            {
                 v.push_back(x);   
            }
            attrs.push_back(v);
        }
    }
}

int main()
{
    readData(labels, attrs);
    learn(labels, attrs);
    return 0;
}
